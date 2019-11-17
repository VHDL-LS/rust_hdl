//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
//!
//! Contains the RpcChannel Traid and associated convenience functions

use languageserver_types::*;
use serde;

pub trait RpcChannel {
    fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    );

    fn window_show_message(&self, typ: MessageType, message: impl Into<String>) {
        self.send_notification(
            "window/showMessage",
            ShowMessageParams {
                typ,
                message: message.into(),
            },
        );
    }
}

#[cfg(test)]
pub mod test_support {

    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;

    #[derive(Debug, Clone)]
    pub enum RpcExpected {
        Notification {
            method: String,
            notification: serde_json::Value,
        },
        /// Check that the string representation of the notification contains a string
        NotificationContainsString { method: String, contains: String },
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

        pub fn expect_notification_contains(
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
    }

    impl Drop for RpcMock {
        fn drop(&mut self) {
            if !std::thread::panicking() {
                let expected = self.expected.replace(VecDeque::new());
                if expected.len() > 0 {
                    panic!("Not all expected data was consumed\n{:#?}", expected);
                }
            }
        }
    }

    impl super::RpcChannel for RpcMock {
        fn send_notification(
            &self,
            method: impl Into<String>,
            notification: impl serde::ser::Serialize,
        ) {
            let method = method.into();
            let notification = serde_json::to_value(notification).unwrap();
            let expected = self
                .expected
                .borrow_mut()
                .pop_front()
                .ok_or_else(|| {
                    panic!(
                        "No expected value, got method={} {:?}",
                        method, notification
                    )
                })
                .unwrap();

            match expected {
                RpcExpected::Notification {
                    method: exp_method,
                    notification: exp_notification,
                } => {
                    assert_eq!(method, exp_method);
                    assert_eq!(notification, exp_notification);
                }
                RpcExpected::NotificationContainsString {
                    method: exp_method,
                    contains,
                } => {
                    assert_eq!(method, exp_method);
                    if !notification.to_string().contains(&contains) {
                        panic!("{:?} does not contain string {:?}", notification, contains);
                    }
                }
            }
        }
    }
}
