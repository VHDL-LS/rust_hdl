// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Contains the RpcChannel Traid and associated convenience functions

use serde_json::Value;
use std::rc::Rc;

pub trait RpcChannel {
    /// Send notification to the client.
    fn send_notification(&self, method: String, notification: Value);

    /// Send request to the client.
    fn send_request(&self, method: String, params: Value);
}

#[derive(Clone)]
pub struct SharedRpcChannel {
    chan: Rc<dyn RpcChannel>,
}

impl SharedRpcChannel {
    pub fn new(chan: Rc<dyn RpcChannel>) -> Self {
        Self { chan }
    }

    /// Send notification to the client.
    pub fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    ) {
        self.chan
            .send_notification(method.into(), serde_json::to_value(&notification).unwrap())
    }

    /// Send request to the client.
    pub fn send_request(&self, method: impl Into<String>, params: impl serde::ser::Serialize) {
        self.chan
            .send_request(method.into(), serde_json::to_value(&params).unwrap())
    }
}

#[cfg(test)]
pub mod test_support {

    use pretty_assertions::assert_eq;
    use serde_json::Value;
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;

    #[derive(Debug)]
    pub enum RpcExpected {
        Notification {
            method: String,
            notification: serde_json::Value,
        },
        /// Check that the string representation of the notification contains a string
        NotificationContainsString { method: String, contains: String },
        Request {
            method: String,
            params: serde_json::Value,
        },
    }

    #[derive(Clone)]
    pub struct RpcMock {
        expected: Rc<RefCell<VecDeque<RpcExpected>>>,
    }

    impl RpcMock {
        pub fn new() -> RpcMock {
            RpcMock {
                expected: Rc::new(RefCell::new(VecDeque::new())),
            }
        }

        pub fn expect_notification(
            &self,
            method: impl Into<String>,
            notification: impl serde::ser::Serialize,
        ) {
            self.expected
                .borrow_mut()
                .push_back(RpcExpected::Notification {
                    method: method.into(),
                    notification: serde_json::to_value(notification).unwrap(),
                });
        }

        fn expect_notification_contains(
            &self,
            method: impl Into<String>,
            contains: impl Into<String>,
        ) {
            self.expected
                .borrow_mut()
                .push_back(RpcExpected::NotificationContainsString {
                    method: method.into(),
                    contains: contains.into(),
                });
        }

        pub fn expect_request(
            &self,
            method: impl Into<String>,
            params: impl serde::ser::Serialize,
        ) {
            self.expected.borrow_mut().push_back(RpcExpected::Request {
                method: method.into(),
                params: serde_json::to_value(params).unwrap(),
            });
        }

        pub fn expect_error_contains(&self, contains: impl Into<String>) {
            let contains = contains.into();
            self.expect_notification_contains("window/showMessage", contains.clone());
            self.expect_notification_contains("window/logMessage", contains);
        }

        pub fn expect_warning_contains(&self, contains: impl Into<String>) {
            self.expect_error_contains(contains)
        }

        pub fn expect_message_contains(&self, contains: impl Into<String>) {
            let contains = contains.into();
            self.expect_notification_contains("window/logMessage", contains);
        }
    }

    impl Drop for RpcMock {
        fn drop(&mut self) {
            if !std::thread::panicking() {
                let expected = self.expected.replace(VecDeque::new());
                if !expected.is_empty() {
                    panic!("Not all expected data was consumed\n{expected:#?}");
                }
            }
        }
    }

    /// True if any string field of the value has string as a substring
    fn contains_string(value: &serde_json::Value, string: &str) -> bool {
        match value {
            serde_json::Value::Array(values) => {
                values.iter().any(|value| contains_string(value, string))
            }
            serde_json::Value::Object(map) => {
                map.values().any(|value| contains_string(value, string))
            }
            serde_json::Value::String(got_string) => got_string.contains(string),
            serde_json::Value::Null => false,
            serde_json::Value::Bool(..) => false,
            serde_json::Value::Number(..) => false,
        }
    }

    impl super::RpcChannel for RpcMock {
        fn send_notification(&self, method: String, notification: Value) {
            let expected = self
                .expected
                .borrow_mut()
                .pop_front()
                .ok_or_else(|| {
                    panic!("No expected value, got notification method={method} {notification:?}")
                })
                .unwrap();

            match expected {
                RpcExpected::Notification {
                    method: exp_method,
                    notification: exp_notification,
                } => {
                    assert_eq!((method, notification), (exp_method, exp_notification));
                }
                RpcExpected::NotificationContainsString {
                    method: exp_method,
                    contains,
                } => {
                    assert_eq!(
                        method, exp_method,
                        "{:?} contains {:?}",
                        notification, contains
                    );
                    if !contains_string(&notification, &contains) {
                        panic!("{notification:?} does not contain sub-string {contains:?}");
                    }
                }
                _ => panic!("Expected {expected:?}, got notification {method} {notification:?}"),
            }
        }

        fn send_request(&self, method: String, params: Value) {
            let expected = self
                .expected
                .borrow_mut()
                .pop_front()
                .ok_or_else(|| panic!("No expected value, got request method={method} {params:?}"))
                .unwrap();

            match expected {
                RpcExpected::Request {
                    method: exp_method,
                    params: exp_params,
                } => {
                    assert_eq!((method, params), (exp_method, exp_params));
                }
                _ => panic!("Expected {expected:?}, got request {method} {params:?}"),
            }
        }
    }
}
