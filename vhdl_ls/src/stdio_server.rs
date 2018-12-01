// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate jsonrpc_core;
extern crate languageserver_types;
extern crate serde;
extern crate serde_json;

use self::jsonrpc_core::request::Notification;
use self::jsonrpc_core::{IoHandler, Params};
use std::io::prelude::*;
use std::io::{self, BufRead};

use std::sync::mpsc::{sync_channel, SyncSender};
use std::thread::spawn;

use std::sync::{Arc, Mutex};

use vhdl_server::{RpcChannel, VHDLServer};

pub fn start() {
    let (request_sender, request_receiver) = sync_channel(1);
    let (response_sender, response_receiver) = sync_channel(1);
    let mut io = IoHandler::new();

    // @TODO handle jsonrpc synchronously
    let lang_server = Arc::new(Mutex::new(VHDLServer::new(response_sender.clone())));
    let server = lang_server.clone();
    io.add_method("initialize", move |params: Params| {
        let result = server.lock().unwrap().initialize_request(params.parse()?)?;
        // @TODO log error
        Ok(serde_json::to_value(result).map_err(|_| jsonrpc_core::Error::internal_error())?)
    });

    let server = lang_server.clone();
    io.add_method("shutdown", move |params: Params| {
        let result = server.lock().unwrap().shutdown_server(params.parse()?)?;
        Ok(serde_json::to_value(result).map_err(|_| jsonrpc_core::Error::internal_error())?)
    });

    let server = lang_server.clone();
    io.add_notification("initialized", move |params: Params| {
        server
            .lock()
            .unwrap()
            .initialized_notification(params.parse().unwrap())
    });

    let server = lang_server.clone();
    io.add_notification("exit", move |params: Params| {
        server
            .lock()
            .unwrap()
            .exit_notification(params.parse().unwrap())
    });

    let server = lang_server.clone();
    io.add_notification("textDocument/didChange", move |params: Params| {
        server
            .lock()
            .unwrap()
            .text_document_did_change_notification(params.parse().unwrap())
    });

    let server = lang_server.clone();
    io.add_notification("textDocument/didOpen", move |params: Params| {
        server
            .lock()
            .unwrap()
            .text_document_did_open_notification(params.parse().unwrap())
    });

    // Spawn thread to read requests from stdin
    spawn(move || {
        let stdin = io::stdin();
        loop {
            let request = read_request(&mut stdin.lock());
            match request_sender.send(request) {
                Ok(_) => continue,
                Err(_) => {
                    eprintln!("Channel hung up. Unlocking stdin handle.");
                    break;
                }
            }
        }
    });

    // Spawn thread to write notifications to stdout
    spawn(move || {
        let mut stdout = io::stdout();
        loop {
            match response_receiver.recv() {
                Ok(response) => {
                    send_response(&mut stdout, &response);
                }
                Err(_) => {
                    eprintln!("Channel hung up.");
                    break;
                }
            }
        }
    });

    loop {
        match request_receiver.recv() {
            Ok(request) => {
                let response = io.handle_request_sync(&request);
                if let Some(response) = response {
                    response_sender.send(response).unwrap();
                }
            }
            Err(_) => {
                eprintln!("Channel hung up.");
                break;
            }
        }
    }
}

fn read_request(reader: &mut BufRead) -> String {
    let content_length = read_header(reader);

    let mut request = String::new();
    reader
        .take(content_length)
        .read_to_string(&mut request)
        .unwrap();
    eprintln!("DEBUG GOT REQUEST: {:?}", request);
    request
}

fn send_response(writer: &mut Write, response: &str) {
    eprintln!("DEBUG SEND RESPONSE: {:?}", response);
    write!(writer, "Content-Length: {}\r\n", response.len());
    write!(writer, "\r\n");
    write!(writer, "{}", response);
    writer.flush().ok().expect("Could not flush stdout");
}

impl RpcChannel for SyncSender<String> {
    fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    ) {
        let params_json = match serde_json::to_value(notification).unwrap() {
            serde_json::Value::Object(map) => map,
            map => panic!("{:?}", map),
        };

        let notification_json = Notification {
            jsonrpc: Some(jsonrpc_core::Version::V2),
            method: method.into(),
            params: Params::Map(params_json),
        };

        self.send(serde_json::to_string(&notification_json).unwrap())
            .unwrap();
    }
}

fn read_header(reader: &mut BufRead) -> u64 {
    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    let fields = buffer.trim_end().clone().split(": ").collect::<Vec<&str>>();
    if fields.get(0) != Some(&"Content-Length") {
        eprintln!("{:?}", fields);
        panic!();
    }
    let content_length = fields.get(1).unwrap().parse::<u64>().unwrap();

    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    if buffer == "\r\n" {
        return content_length;
    }

    let fields = buffer.trim_end().clone().split(": ").collect::<Vec<&str>>();
    if fields.get(0) != Some(&"Content-Type") {
        eprintln!("{:?}", fields);
        panic!();
    } else {
        eprintln!("got Content-Type: {}", fields.get(1).unwrap());
    }

    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    if buffer != "\r\n" {
        eprintln!("{:?}", buffer);
        panic!();
    }

    return content_length;
}
