/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use cors::CORSResponse;
use cors::{AsyncCORSResponseListener, CORSRequest, RequestMode, allow_cross_origin_request};
use document_loader::DocumentLoader;
use dom::bindings::cell::DOMRefCell;
use dom::bindings::codegen::Bindings::BlobBinding::BlobMethods;
use dom::bindings::codegen::Bindings::EventHandlerBinding::EventHandlerNonNull;
use dom::bindings::codegen::Bindings::WindowBinding::WindowMethods;
use dom::bindings::codegen::Bindings::XMLHttpRequestBinding;
use dom::bindings::codegen::Bindings::XMLHttpRequestBinding::XMLHttpRequestMethods;
use dom::bindings::codegen::Bindings::XMLHttpRequestBinding::XMLHttpRequestResponseType;
use dom::bindings::codegen::Bindings::XMLHttpRequestBinding::XMLHttpRequestResponseType::{Json, Text, _empty};
use dom::bindings::codegen::UnionTypes::BlobOrStringOrURLSearchParams;
use dom::bindings::conversions::{ToJSValConvertible};
use dom::bindings::error::{Error, ErrorResult, Fallible};
use dom::bindings::global::{GlobalRef, GlobalRoot};
use dom::bindings::inheritance::Castable;
use dom::bindings::js::{JS, MutNullableHeap};
use dom::bindings::js::{Root, RootedReference};
use dom::bindings::refcounted::Trusted;
use dom::bindings::reflector::{Reflectable, reflect_dom_object};
use dom::bindings::str::{ByteString, USVString};
use dom::document::DocumentSource;
use dom::document::{Document, IsHTMLDocument};
use dom::event::{Event, EventBubbles, EventCancelable};
use dom::eventtarget::EventTarget;
use dom::progressevent::ProgressEvent;
use dom::xmlhttprequesteventtarget::XMLHttpRequestEventTarget;
use dom::xmlhttprequestupload::XMLHttpRequestUpload;
use encoding::all::UTF_8;
use encoding::label::encoding_from_whatwg_label;
use encoding::types::{DecoderTrap, EncoderTrap, Encoding, EncodingRef};
use euclid::length::Length;
use hyper::header::Headers;
use hyper::header::{Accept, ContentLength, ContentType, qitem};
use hyper::http::RawStatus;
use hyper::method::Method;
use hyper::mime::{self, Mime};
use ipc_channel::ipc;
use ipc_channel::router::ROUTER;
use js::jsapi::JS_ClearPendingException;
use js::jsapi::{JSContext, JS_ParseJSON, RootedValue};
use js::jsval::{JSVal, NullValue, UndefinedValue};
use net_traits::ControlMsg::Load;
use net_traits::{AsyncResponseListener, AsyncResponseTarget, Metadata};
use net_traits::{LoadConsumer, LoadContext, LoadData, ResourceCORSData, ResourceThread};
use network_listener::{NetworkListener, PreInvoke};
use parse::html::{ParseContext, parse_html};
use parse::xml::{self, parse_xml};
use script_thread::{ScriptChan, ScriptPort};
use std::ascii::AsciiExt;
use std::borrow::ToOwned;
use std::cell::{Cell, RefCell};
use std::default::Default;
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};
use string_cache::Atom;
use time;
use timers::{ScheduledCallback, TimerHandle};
use url::Url;
use util::str::DOMString;

pub type SendParam = BlobOrStringOrURLSearchParams;

#[derive(JSTraceable, PartialEq, Copy, Clone, HeapSizeOf)]
enum XMLHttpRequestState {
    Unsent = 0,
    Opened = 1,
    HeadersReceived = 2,
    Loading = 3,
    Done = 4,
}

#[derive(JSTraceable, PartialEq, Clone, Copy, HeapSizeOf)]
pub struct GenerationId(u32);

/// Closure of required data for each async network event that comprises the
/// XHR's response.
struct XHRContext {
    xhr: TrustedXHRAddress,
    gen_id: GenerationId,
    cors_request: Option<CORSRequest>,
    buf: DOMRefCell<Vec<u8>>,
    sync_status: DOMRefCell<Option<ErrorResult>>,
}

#[derive(Clone)]
pub enum XHRProgress {
    /// Notify that headers have been received
    HeadersReceived(GenerationId, Option<Headers>, Option<RawStatus>),
    /// Partial progress (after receiving headers), containing portion of the response
    Loading(GenerationId, ByteString),
    /// Loading is done
    Done(GenerationId),
    /// There was an error (only Error::Abort, Error::Timeout or Error::Network is used)
    Errored(GenerationId, Error),
}

impl XHRProgress {
    fn generation_id(&self) -> GenerationId {
        match *self {
            XHRProgress::HeadersReceived(id, _, _) |
            XHRProgress::Loading(id, _) |
            XHRProgress::Done(id) |
            XHRProgress::Errored(id, _) => id
        }
    }
}

#[dom_struct]
pub struct XMLHttpRequest {
    eventtarget: XMLHttpRequestEventTarget,
    ready_state: Cell<XMLHttpRequestState>,
    timeout: Cell<u32>,
    with_credentials: Cell<bool>,
    upload: JS<XMLHttpRequestUpload>,
    response_url: String,
    status: Cell<u16>,
    status_text: DOMRefCell<ByteString>,
    response: DOMRefCell<ByteString>,
    response_type: Cell<XMLHttpRequestResponseType>,
    response_xml: MutNullableHeap<JS<Document>>,
    #[ignore_heap_size_of = "Defined in hyper"]
    response_headers: DOMRefCell<Headers>,
    #[ignore_heap_size_of = "Defined in hyper"]
    override_mime_type: DOMRefCell<Option<Mime>>,
    #[ignore_heap_size_of = "Defined in rust-encoding"]
    override_charset: DOMRefCell<Option<EncodingRef>>,

    // Associated concepts
    #[ignore_heap_size_of = "Defined in hyper"]
    request_method: DOMRefCell<Method>,
    request_url: DOMRefCell<Option<Url>>,
    #[ignore_heap_size_of = "Defined in hyper"]
    request_headers: DOMRefCell<Headers>,
    request_body_len: Cell<usize>,
    sync: Cell<bool>,
    upload_complete: Cell<bool>,
    upload_events: Cell<bool>,
    send_flag: Cell<bool>,

    timeout_cancel: DOMRefCell<Option<TimerHandle>>,
    fetch_time: Cell<i64>,
    generation_id: Cell<GenerationId>,
    response_status: Cell<Result<(), ()>>,
}

impl XMLHttpRequest {
    fn new_inherited(global: GlobalRef) -> XMLHttpRequest {
        XMLHttpRequest {
            eventtarget: XMLHttpRequestEventTarget::new_inherited(),
            ready_state: Cell::new(XMLHttpRequestState::Unsent),
            timeout: Cell::new(0u32),
            with_credentials: Cell::new(false),
            upload: JS::from_rooted(&XMLHttpRequestUpload::new(global)),
            response_url: String::from(""),
            status: Cell::new(0),
            status_text: DOMRefCell::new(ByteString::new(vec!())),
            response: DOMRefCell::new(ByteString::new(vec!())),
            response_type: Cell::new(_empty),
            response_xml: Default::default(),
            response_headers: DOMRefCell::new(Headers::new()),
            override_mime_type: DOMRefCell::new(None),
            override_charset: DOMRefCell::new(None),

            request_method: DOMRefCell::new(Method::Get),
            request_url: DOMRefCell::new(None),
            request_headers: DOMRefCell::new(Headers::new()),
            request_body_len: Cell::new(0),
            sync: Cell::new(false),
            upload_complete: Cell::new(false),
            upload_events: Cell::new(false),
            send_flag: Cell::new(false),

            timeout_cancel: DOMRefCell::new(None),
            fetch_time: Cell::new(0),
            generation_id: Cell::new(GenerationId(0)),
            response_status: Cell::new(Ok(())),
        }
    }
    pub fn new(global: GlobalRef) -> Root<XMLHttpRequest> {
        reflect_dom_object(box XMLHttpRequest::new_inherited(global),
                           global,
                           XMLHttpRequestBinding::Wrap)
    }

    // https://xhr.spec.whatwg.org/#constructors
    pub fn Constructor(global: GlobalRef) -> Fallible<Root<XMLHttpRequest>> {
        Ok(XMLHttpRequest::new(global))
    }

    fn check_cors(context: Arc<Mutex<XHRContext>>,
                  load_data: LoadData,
                  req: CORSRequest,
                  script_chan: Box<ScriptChan + Send>,
                  resource_thread: ResourceThread) {
        struct CORSContext {
            xhr: Arc<Mutex<XHRContext>>,
            load_data: RefCell<Option<LoadData>>,
            req: CORSRequest,
            script_chan: Box<ScriptChan + Send>,
            resource_thread: ResourceThread,
        }

        impl AsyncCORSResponseListener for CORSContext {
            fn response_available(&self, response: CORSResponse) {
                if response.network_error {
                    let mut context = self.xhr.lock().unwrap();
                    let xhr = context.xhr.root();
                    xhr.process_partial_response(XHRProgress::Errored(context.gen_id, Error::Network));
                    *context.sync_status.borrow_mut() = Some(Err(Error::Network));
                    return;
                }

                let mut load_data = self.load_data.borrow_mut().take().unwrap();
                load_data.cors = Some(ResourceCORSData {
                    preflight: self.req.preflight_flag,
                    origin: self.req.origin.clone()
                });

                XMLHttpRequest::initiate_async_xhr(self.xhr.clone(), self.script_chan.clone(),
                                                   self.resource_thread.clone(), load_data);
            }
        }

        let cors_context = CORSContext {
            xhr: context,
            load_data: RefCell::new(Some(load_data)),
            req: req.clone(),
            script_chan: script_chan.clone(),
            resource_thread: resource_thread,
        };

        req.http_fetch_async(box cors_context, script_chan);
    }

    fn initiate_async_xhr(context: Arc<Mutex<XHRContext>>,
                          script_chan: Box<ScriptChan + Send>,
                          resource_thread: ResourceThread,
                          load_data: LoadData) {
        impl AsyncResponseListener for XHRContext {
            fn headers_available(&mut self, metadata: Metadata) {
                let xhr = self.xhr.root();
                let rv = xhr.process_headers_available(self.cors_request.clone(),
                                                       self.gen_id,
                                                       metadata);
                if rv.is_err() {
                    *self.sync_status.borrow_mut() = Some(rv);
                }
            }

            fn data_available(&mut self, payload: Vec<u8>) {
                self.buf.borrow_mut().extend_from_slice(&payload);
                self.xhr.root().process_data_available(self.gen_id, self.buf.borrow().clone());
            }

            fn response_complete(&mut self, status: Result<(), String>) {
                let rv = self.xhr.root().process_response_complete(self.gen_id, status);
                *self.sync_status.borrow_mut() = Some(rv);
            }
        }

        impl PreInvoke for XHRContext {
            fn should_invoke(&self) -> bool {
                self.xhr.root().generation_id.get() == self.gen_id
            }
        }

        let (action_sender, action_receiver) = ipc::channel().unwrap();
        let listener = NetworkListener {
            context: context,
            script_chan: script_chan,
        };
        let response_target = AsyncResponseTarget {
            sender: action_sender,
        };
        ROUTER.add_route(action_receiver.to_opaque(), box move |message| {
            listener.notify(message.to().unwrap());
        });
        resource_thread.send(Load(load_data, LoadConsumer::Listener(response_target), None)).unwrap();
    }
}

impl XMLHttpRequestMethods for XMLHttpRequest {
    // https://xhr.spec.whatwg.org/#handler-xhr-onreadystatechange
    event_handler!(readystatechange, GetOnreadystatechange, SetOnreadystatechange);

    // https://xhr.spec.whatwg.org/#dom-xmlhttprequest-readystate
    fn ReadyState(&self) -> u16 {
        self.ready_state.get() as u16
    }

    // https://xhr.spec.whatwg.org/#the-open()-method
    fn Open(&self, method: ByteString, url: USVString) -> ErrorResult {
        //FIXME(seanmonstar): use a Trie instead?
        let maybe_method = method.as_str().and_then(|s| {
            // Note: hyper tests against the uppercase versions
            // Since we want to pass methods not belonging to the short list above
            // without changing capitalization, this will actually sidestep rust-http's type system
            // since methods like "patch" or "PaTcH" will be considered extension methods
            // despite the there being a rust-http method variant for them
            let upper = s.to_ascii_uppercase();
            match &*upper {
                "DELETE" | "GET" | "HEAD" | "OPTIONS" |
                "POST" | "PUT" | "CONNECT" | "TRACE" |
                "TRACK" => upper.parse().ok(),
                _ => s.parse().ok()
            }
        });
        // Step 2
        match maybe_method {
            // Step 4
            Some(Method::Connect) | Some(Method::Trace) => Err(Error::Security),
            Some(Method::Extension(ref t)) if &**t == "TRACK" => Err(Error::Security),
            Some(parsed_method) => {
                // Step 3
                if !method.is_token() {
                    return Err(Error::Syntax)
                }

                *self.request_method.borrow_mut() = parsed_method;

                // Step 6
                let base = self.global().r().get_url();
                let parsed_url = match base.join(&url.0) {
                    Ok(parsed) => parsed,
                    Err(_) => return Err(Error::Syntax) // Step 7
                };
                // XXXManishearth Do some handling of username/passwords
                if self.sync.get() {
                    // FIXME: This should only happen if the global environment is a document environment
                    if self.timeout.get() != 0 || self.with_credentials.get() || self.response_type.get() != _empty {
                        return Err(Error::InvalidAccess)
                    }
                }
                // abort existing requests
                self.terminate_ongoing_fetch();

                // Step 12
                *self.request_url.borrow_mut() = Some(parsed_url);
                *self.request_headers.borrow_mut() = Headers::new();
                self.send_flag.set(false);
                *self.status_text.borrow_mut() = ByteString::new(vec!());
                self.status.set(0);

                // Step 13
                if self.ready_state.get() != XMLHttpRequestState::Opened {
                    self.change_ready_state(XMLHttpRequestState::Opened);
                }
                Ok(())
            },
            // This includes cases where as_str() returns None, and when is_token() returns false,
            // both of which indicate invalid extension method names
            _ => Err(Error::Syntax), // Step 3
        }
    }

    // https://xhr.spec.whatwg.org/#the-open()-method
    fn Open_(&self, method: ByteString, url: USVString, async: bool,
                 _username: Option<USVString>, _password: Option<USVString>) -> ErrorResult {
        self.sync.set(!async);
        self.Open(method, url)
    }

    // https://xhr.spec.whatwg.org/#the-setrequestheader()-method
    fn SetRequestHeader(&self, name: ByteString, mut value: ByteString) -> ErrorResult {
        if self.ready_state.get() != XMLHttpRequestState::Opened || self.send_flag.get() {
            return Err(Error::InvalidState); // Step 1, 2
        }
        if !name.is_token() || !value.is_field_value() {
            return Err(Error::Syntax); // Step 3, 4
        }
        let name_lower = name.to_lower();
        let name_str = match name_lower.as_str() {
            Some(s) => {
                match s {
                    // Disallowed headers
                    "accept-charset" | "accept-encoding" |
                    "access-control-request-headers" |
                    "access-control-request-method" |
                    "connection" | "content-length" |
                    "cookie" | "cookie2" | "date" |"dnt" |
                    "expect" | "host" | "keep-alive" | "origin" |
                    "referer" | "te" | "trailer" | "transfer-encoding" |
                    "upgrade" | "user-agent" | "via" => {
                        return Ok(()); // Step 5
                    },
                    _ => s
                }
            },
            None => return Err(Error::Syntax)
        };

        debug!("SetRequestHeader: name={:?}, value={:?}", name.as_str(), value.as_str());
        let mut headers = self.request_headers.borrow_mut();


        // Steps 6,7
        match headers.get_raw(name_str) {
            Some(raw) => {
                debug!("SetRequestHeader: old value = {:?}", raw[0]);
                let mut buf = raw[0].clone();
                buf.extend_from_slice(b", ");
                buf.extend_from_slice(&value);
                debug!("SetRequestHeader: new value = {:?}", buf);
                value = ByteString::new(buf);
            },
            None => {}
        }

        headers.set_raw(name_str.to_owned(), vec![value.to_vec()]);
        Ok(())
    }

    // https://xhr.spec.whatwg.org/#the-timeout-attribute
    fn Timeout(&self) -> u32 {
        self.timeout.get()
    }

    // https://xhr.spec.whatwg.org/#the-timeout-attribute
    fn SetTimeout(&self, timeout: u32) -> ErrorResult {
        if self.sync.get() {
            // FIXME: Not valid for a worker environment
            Err(Error::InvalidAccess)
        } else {
            self.timeout.set(timeout);
            if self.send_flag.get() {
                if timeout == 0 {
                    self.cancel_timeout();
                    return Ok(());
                }
                let progress = time::now().to_timespec().sec - self.fetch_time.get();
                if timeout > (progress * 1000) as u32 {
                    self.set_timeout(timeout - (progress * 1000) as u32);
                } else {
                    // Immediately execute the timeout steps
                    self.set_timeout(0);
                }
            }
            Ok(())
        }
    }

    // https://xhr.spec.whatwg.org/#the-withcredentials-attribute
    fn WithCredentials(&self) -> bool {
        self.with_credentials.get()
    }

    // https://xhr.spec.whatwg.org/#dom-xmlhttprequest-withcredentials
    fn SetWithCredentials(&self, with_credentials: bool) -> ErrorResult {
        match self.ready_state.get() {
            XMLHttpRequestState::HeadersReceived |
            XMLHttpRequestState::Loading |
            XMLHttpRequestState::Done => Err(Error::InvalidState),
            _ if self.send_flag.get() => Err(Error::InvalidState),
            _ => match self.global() {
                GlobalRoot::Window(_) if self.sync.get() => Err(Error::InvalidAccess),
                _ => {
                    self.with_credentials.set(with_credentials);
                    Ok(())
                },
            },
        }
    }

    // https://xhr.spec.whatwg.org/#the-upload-attribute
    fn Upload(&self) -> Root<XMLHttpRequestUpload> {
        Root::from_ref(&*self.upload)
    }

    // https://xhr.spec.whatwg.org/#the-send()-method
    fn Send(&self, data: Option<SendParam>) -> ErrorResult {
        if self.ready_state.get() != XMLHttpRequestState::Opened || self.send_flag.get() {
            return Err(Error::InvalidState); // Step 1, 2
        }

        let data = match *self.request_method.borrow() {
            Method::Get | Method::Head => None, // Step 3
            _ => data
        };
        let extracted = data.as_ref().map(|d| d.extract());
        self.request_body_len.set(extracted.as_ref().map_or(0, |e| e.0.len()));

        // Step 6
        self.upload_events.set(false);
        // Step 7
        self.upload_complete.set(match extracted {
            None => true,
            Some (ref e) if e.0.is_empty() => true,
            _ => false
        });

        if !self.sync.get() {
            // Step 8
            let event_target = self.upload.upcast::<EventTarget>();
            if event_target.has_handlers() {
                self.upload_events.set(true);
            }

            // Step 9
            self.send_flag.set(true);
            // If one of the event handlers below aborts the fetch by calling
            // abort or open we will need the current generation id to detect it.
            let gen_id = self.generation_id.get();
            self.dispatch_response_progress_event(atom!("loadstart"));
            if self.generation_id.get() != gen_id {
                return Ok(());
            }
            if !self.upload_complete.get() {
                self.dispatch_upload_progress_event(atom!("loadstart"), Some(0));
                if self.generation_id.get() != gen_id {
                    return Ok(());
                }
            }

        }

        let global = self.global();
        let pipeline_id = global.r().pipeline();
        let mut load_data =
            LoadData::new(LoadContext::Browsing,
                          self.request_url.borrow().clone().unwrap(),
                          Some(pipeline_id));
        if load_data.url.origin().ne(&global.r().get_url().origin()) {
            load_data.credentials_flag = self.WithCredentials();
        }
        load_data.data = extracted.as_ref().map(|e| e.0.clone());

        // XHR spec differs from http, and says UTF-8 should be in capitals,
        // instead of "utf-8", which is what Hyper defaults to. So not
        // using content types provided by Hyper.
        let n = "content-type";
        match extracted {
            Some((_, Some(ref content_type))) =>
                load_data.headers.set_raw(n.to_owned(), vec![content_type.bytes().collect()]),
            _ => (),
        }

        load_data.preserved_headers = (*self.request_headers.borrow()).clone();

        if !load_data.preserved_headers.has::<Accept>() {
            let mime = Mime(mime::TopLevel::Star, mime::SubLevel::Star, vec![]);
            load_data.preserved_headers.set(Accept(vec![qitem(mime)]));
        }

        load_data.method = (*self.request_method.borrow()).clone();

        // CORS stuff
        let global = self.global();
        let referer_url = self.global().r().get_url();
        let mode = if self.upload_events.get() {
            RequestMode::ForcedPreflight
        } else {
            RequestMode::CORS
        };
        let mut combined_headers = load_data.headers.clone();
        combined_headers.extend(load_data.preserved_headers.iter());
        let cors_request = CORSRequest::maybe_new(referer_url.clone(),
                                                  load_data.url.clone(),
                                                  mode,
                                                  load_data.method.clone(),
                                                  combined_headers);
        match cors_request {
            Ok(None) => {
                let mut buf = String::new();
                buf.push_str(&referer_url.scheme);
                buf.push_str("://");

                if let Some(ref h) = referer_url.serialize_host() {
                    buf.push_str(h);
                }

                if let Some(ref p) = referer_url.port().as_ref() {
                    buf.push_str(":");
                    buf.push_str(&p.to_string());
                }

                if let Some(ref h) = referer_url.serialize_path() {
                    buf.push_str(h);
                }

                self.request_headers.borrow_mut().set_raw("Referer".to_owned(), vec![buf.into_bytes()]);
            },
            Ok(Some(ref req)) => self.insert_trusted_header("origin".to_owned(),
                                                            req.origin.to_string()),
            _ => {}
        }

        debug!("request_headers = {:?}", *self.request_headers.borrow());

        self.fetch_time.set(time::now().to_timespec().sec);
        let rv = self.fetch(load_data, cors_request, global.r());
        if self.sync.get() {
            return rv;
        }

        let timeout = self.timeout.get();
        if timeout > 0 {
            self.set_timeout(timeout);
        }
        Ok(())
    }

    // https://xhr.spec.whatwg.org/#the-abort()-method
    fn Abort(&self) {
        self.terminate_ongoing_fetch();
        let state = self.ready_state.get();
        if (state == XMLHttpRequestState::Opened && self.send_flag.get()) ||
           state == XMLHttpRequestState::HeadersReceived ||
           state == XMLHttpRequestState::Loading {
            let gen_id = self.generation_id.get();
            self.process_partial_response(XHRProgress::Errored(gen_id, Error::Abort));
            // If open was called in one of the handlers invoked by the
            // above call then we should terminate the abort sequence
            if self.generation_id.get() != gen_id {
                return
            }
        }
        self.ready_state.set(XMLHttpRequestState::Unsent);
    }

    // https://xhr.spec.whatwg.org/#the-responseurl-attribute
    fn ResponseURL(&self) -> USVString {
        USVString(self.response_url.clone())
    }

    // https://xhr.spec.whatwg.org/#the-status-attribute
    fn Status(&self) -> u16 {
        self.status.get()
    }

    // https://xhr.spec.whatwg.org/#the-statustext-attribute
    fn StatusText(&self) -> ByteString {
        self.status_text.borrow().clone()
    }

    // https://xhr.spec.whatwg.org/#the-getresponseheader()-method
    fn GetResponseHeader(&self, name: ByteString) -> Option<ByteString> {
        self.filter_response_headers().iter().find(|h| {
            name.eq_ignore_case(&h.name().parse().unwrap())
        }).map(|h| {
            ByteString::new(h.value_string().into_bytes())
        })
    }

    // https://xhr.spec.whatwg.org/#the-getallresponseheaders()-method
    fn GetAllResponseHeaders(&self) -> ByteString {
        ByteString::new(self.filter_response_headers().to_string().into_bytes())
    }

    // https://xhr.spec.whatwg.org/#the-overridemimetype()-method
    fn OverrideMimeType(&self, mime: DOMString) -> ErrorResult {
        match self.ready_state.get() {
            XMLHttpRequestState::Loading | XMLHttpRequestState::Done => return Err(Error::InvalidState),
            _ => {},
        }
        let override_mime = try!(mime.parse::<Mime>().map_err(|_| Error::Syntax));
        *self.override_mime_type.borrow_mut() = Some(override_mime.clone());
        let value = override_mime.get_param(mime::Attr::Charset);
        *self.override_charset.borrow_mut() = value.and_then(|value| {
                encoding_from_whatwg_label(value)
        });
        Ok(())
    }

    // https://xhr.spec.whatwg.org/#the-responsetype-attribute
    fn ResponseType(&self) -> XMLHttpRequestResponseType {
        self.response_type.get()
    }

    // https://xhr.spec.whatwg.org/#the-responsetype-attribute
    fn SetResponseType(&self, response_type: XMLHttpRequestResponseType) -> ErrorResult {
        match self.global() {
            GlobalRoot::Worker(_) if response_type == XMLHttpRequestResponseType::Document => return Ok(()),
            _ => {}
        }
        match self.ready_state.get() {
            XMLHttpRequestState::Loading | XMLHttpRequestState::Done => Err(Error::InvalidState),
            _ => {
                if let (GlobalRoot::Window(_), true) = (self.global(), self.sync.get()) {
                    Err(Error::InvalidAccess)
                } else {
                    self.response_type.set(response_type);
                    Ok(())
                }
            }
        }
    }

    #[allow(unsafe_code)]
    // https://xhr.spec.whatwg.org/#the-response-attribute
    fn Response(&self, cx: *mut JSContext) -> JSVal {
        unsafe {
            let mut rval = RootedValue::new(cx, UndefinedValue());
            match self.response_type.get() {
                _empty | Text => {
                    let ready_state = self.ready_state.get();
                    if ready_state == XMLHttpRequestState::Done || ready_state == XMLHttpRequestState::Loading {
                        self.text_response().to_jsval(cx, rval.handle_mut());
                    } else {
                        "".to_jsval(cx, rval.handle_mut());
                    }
                },
                _ if self.ready_state.get() != XMLHttpRequestState::Done => {
                    return NullValue()
                },
                XMLHttpRequestResponseType::Document => {
                    let op_doc = self.GetResponseXML();
                    if let Ok(Some(doc)) = op_doc {
                        doc.to_jsval(cx, rval.handle_mut());
                    } else {
                        return NullValue();
                    }
                },
                Json => {
                    let decoded = UTF_8.decode(&self.response.borrow(), DecoderTrap::Replace).unwrap().to_owned();
                    let decoded: Vec<u16> = decoded.utf16_units().collect();
                    if !JS_ParseJSON(cx,
                                     decoded.as_ptr(),
                                     decoded.len() as u32,
                                     rval.handle_mut()) {
                        JS_ClearPendingException(cx);
                        return NullValue();
                    }
                }
                _ => {
                    // XXXManishearth handle other response types
                    self.response.borrow().to_jsval(cx, rval.handle_mut());
                }
            }
            rval.ptr
        }
    }

    // https://xhr.spec.whatwg.org/#the-responsetext-attribute
    fn GetResponseText(&self) -> Fallible<USVString> {
        match self.response_type.get() {
            _empty | Text => {
                Ok(USVString(String::from(match self.ready_state.get() {
                    XMLHttpRequestState::Loading | XMLHttpRequestState::Done => self.text_response(),
                    _ => "".to_owned()
                })))
            },
            _ => Err(Error::InvalidState)
        }
    }

    // https://xhr.spec.whatwg.org/#the-responsexml-attribute
    fn GetResponseXML(&self) -> Fallible<Option<Root<Document>>> {
        match self.response_type.get() {
            _empty | XMLHttpRequestResponseType::Document => {
                match self.ready_state.get() {
                    XMLHttpRequestState::Done => {
                        match self.response_xml.get() {
                            Some(response) => Ok(Some(response)),
                            None => {
                                let response = self.document_response();
                                self.response_xml.set(response.r());
                                Ok(response)
                            }
                        }
                    },
                    _ => Ok(None)
                }
            },
            _ => { Err(Error::InvalidState) }
        }
    }
}

pub type TrustedXHRAddress = Trusted<XMLHttpRequest>;


impl XMLHttpRequest {
    fn change_ready_state(&self, rs: XMLHttpRequestState) {
        assert!(self.ready_state.get() != rs);
        self.ready_state.set(rs);
        let global = self.global();
        let event = Event::new(global.r(),
                               atom!("readystatechange"),
                               EventBubbles::DoesNotBubble,
                               EventCancelable::Cancelable);
        event.fire(self.upcast());
    }

    fn process_headers_available(&self, cors_request: Option<CORSRequest>,
                                 gen_id: GenerationId, metadata: Metadata) -> Result<(), Error> {

        if let Some(ref req) = cors_request {
            match metadata.headers {
                Some(ref h) if allow_cross_origin_request(req, h) => {},
                _ => {
                    self.process_partial_response(XHRProgress::Errored(gen_id, Error::Network));
                    return Err(Error::Network);
                }
            }
        }

        // XXXManishearth Clear cache entries in case of a network error
        self.process_partial_response(XHRProgress::HeadersReceived(gen_id,
                                                                   metadata.headers,
                                                                   metadata.status));
        Ok(())
    }

    fn process_data_available(&self, gen_id: GenerationId, payload: Vec<u8>) {
        self.process_partial_response(XHRProgress::Loading(gen_id, ByteString::new(payload)));
    }

    fn process_response_complete(&self, gen_id: GenerationId, status: Result<(), String>)
                                 -> ErrorResult {
        match status {
            Ok(()) => {
                self.process_partial_response(XHRProgress::Done(gen_id));
                Ok(())
            },
            Err(_) => {
                self.process_partial_response(XHRProgress::Errored(gen_id, Error::Network));
                Err(Error::Network)
            }
        }
    }

    fn process_partial_response(&self, progress: XHRProgress) {
        let msg_id = progress.generation_id();

        // Aborts processing if abort() or open() was called
        // (including from one of the event handlers called below)
        macro_rules! return_if_fetch_was_terminated(
            () => (
                if msg_id != self.generation_id.get() {
                    return
                }
            );
        );

        // Ignore message if it belongs to a terminated fetch
        return_if_fetch_was_terminated!();

        // Ignore messages coming from previously-errored responses or requests that have timed out
        if self.response_status.get().is_err() {
            return;
        }

        match progress {
            XHRProgress::HeadersReceived(_, headers, status) => {
                assert!(self.ready_state.get() == XMLHttpRequestState::Opened);
                // For synchronous requests, this should not fire any events, and just store data
                // XXXManishearth Find a way to track partial progress of the send (onprogresss for XHRUpload)

                // Part of step 13, send() (processing request end of file)
                // Substep 1
                self.upload_complete.set(true);
                // Substeps 2-4
                if !self.sync.get() {
                    self.dispatch_upload_progress_event(atom!("progress"), None);
                    return_if_fetch_was_terminated!();
                    self.dispatch_upload_progress_event(atom!("load"), None);
                    return_if_fetch_was_terminated!();
                    self.dispatch_upload_progress_event(atom!("loadend"), None);
                    return_if_fetch_was_terminated!();
                }
                // Part of step 13, send() (processing response)
                // XXXManishearth handle errors, if any (substep 1)
                // Substep 2
                status.map(|RawStatus(code, reason)| {
                    self.status.set(code);
                    *self.status_text.borrow_mut() = ByteString::new(reason.into_owned().into_bytes());
                });
                headers.as_ref().map(|h| *self.response_headers.borrow_mut() = h.clone());

                // Substep 3
                if !self.sync.get() {
                    self.change_ready_state(XMLHttpRequestState::HeadersReceived);
                }
            },
            XHRProgress::Loading(_, partial_response) => {
                // For synchronous requests, this should not fire any events, and just store data
                // Part of step 11, send() (processing response body)
                // XXXManishearth handle errors, if any (substep 2)

                *self.response.borrow_mut() = partial_response;
                if !self.sync.get() {
                    if self.ready_state.get() == XMLHttpRequestState::HeadersReceived {
                        self.change_ready_state(XMLHttpRequestState::Loading);
                        return_if_fetch_was_terminated!();
                    }
                    self.dispatch_response_progress_event(atom!("progress"));
                }
            },
            XHRProgress::Done(_) => {
                assert!(self.ready_state.get() == XMLHttpRequestState::HeadersReceived ||
                        self.ready_state.get() == XMLHttpRequestState::Loading ||
                        self.sync.get());

                self.cancel_timeout();

                // Part of step 11, send() (processing response end of file)
                // XXXManishearth handle errors, if any (substep 2)

                // Subsubsteps 5-7
                self.send_flag.set(false);
                self.change_ready_state(XMLHttpRequestState::Done);
                return_if_fetch_was_terminated!();
                // Subsubsteps 10-12
                self.dispatch_response_progress_event(atom!("progress"));
                return_if_fetch_was_terminated!();
                self.dispatch_response_progress_event(atom!("load"));
                return_if_fetch_was_terminated!();
                self.dispatch_response_progress_event(atom!("loadend"));
            },
            XHRProgress::Errored(_, e) => {
                self.cancel_timeout();

                self.discard_subsequent_responses();
                self.send_flag.set(false);
                // XXXManishearth set response to NetworkError
                self.change_ready_state(XMLHttpRequestState::Done);
                return_if_fetch_was_terminated!();

                let errormsg = match e {
                    Error::Abort => "abort",
                    Error::Timeout => "timeout",
                    _ => "error",
                };

                let upload_complete = &self.upload_complete;
                if !upload_complete.get() {
                    upload_complete.set(true);
                    self.dispatch_upload_progress_event(atom!("progress"), None);
                    return_if_fetch_was_terminated!();
                    self.dispatch_upload_progress_event(Atom::from(errormsg), None);
                    return_if_fetch_was_terminated!();
                    self.dispatch_upload_progress_event(atom!("loadend"), None);
                    return_if_fetch_was_terminated!();
                }
                self.dispatch_response_progress_event(atom!("progress"));
                return_if_fetch_was_terminated!();
                self.dispatch_response_progress_event(Atom::from(errormsg));
                return_if_fetch_was_terminated!();
                self.dispatch_response_progress_event(atom!("loadend"));
            }
        }
    }

    fn terminate_ongoing_fetch(&self) {
        let GenerationId(prev_id) = self.generation_id.get();
        self.generation_id.set(GenerationId(prev_id + 1));
        self.response_status.set(Ok(()));
    }

    fn insert_trusted_header(&self, name: String, value: String) {
        // Insert a header without checking spec-compliance
        // Use for hardcoded headers
        self.request_headers.borrow_mut().set_raw(name, vec![value.into_bytes()]);
    }

    fn dispatch_progress_event(&self, upload: bool, type_: Atom, loaded: u64, total: Option<u64>) {
        let global = self.global();
        let progressevent = ProgressEvent::new(global.r(),
                                               type_,
                                               EventBubbles::DoesNotBubble,
                                               EventCancelable::NotCancelable,
                                               total.is_some(), loaded,
                                               total.unwrap_or(0));
        let target = if upload {
            self.upload.upcast()
        } else {
            self.upcast()
        };
        progressevent.upcast::<Event>().fire(target);
    }

    fn dispatch_upload_progress_event(&self, type_: Atom, partial_load: Option<u64>) {
        // If partial_load is None, loading has completed and we can just use the value from the request body

        let total = self.request_body_len.get() as u64;
        self.dispatch_progress_event(true, type_, partial_load.unwrap_or(total), Some(total));
    }

    fn dispatch_response_progress_event(&self, type_: Atom) {
        let len = self.response.borrow().len() as u64;
        let total = self.response_headers.borrow().get::<ContentLength>().map(|x| { **x as u64 });
        self.dispatch_progress_event(false, type_, len, total);
    }
    fn set_timeout(&self, duration_ms: u32) {
        #[derive(JSTraceable, HeapSizeOf)]
        struct ScheduledXHRTimeout {
            #[ignore_heap_size_of = "Because it is non-owning"]
            xhr: Trusted<XMLHttpRequest>,
            generation_id: GenerationId,
        }

        impl ScheduledCallback for ScheduledXHRTimeout {
            fn invoke(self: Box<Self>) {
                let this = *self;
                let xhr = this.xhr.root();
                if xhr.ready_state.get() != XMLHttpRequestState::Done {
                    xhr.process_partial_response(XHRProgress::Errored(this.generation_id, Error::Timeout));
                }
            }

            fn box_clone(&self) -> Box<ScheduledCallback> {
                box ScheduledXHRTimeout {
                    xhr: self.xhr.clone(),
                    generation_id: self.generation_id,
                }
            }
        }

        // Sets up the object to timeout in a given number of milliseconds
        // This will cancel all previous timeouts
        let global = self.global();
        let callback = ScheduledXHRTimeout {
            xhr: Trusted::new(self, global.r().networking_thread_source()),
            generation_id: self.generation_id.get(),
        };
        let duration = Length::new(duration_ms as u64);
        *self.timeout_cancel.borrow_mut() = Some(global.r().schedule_callback(box callback, duration));
    }

    fn cancel_timeout(&self) {
        if let Some(handle) = self.timeout_cancel.borrow_mut().take() {
            let global = self.global();
            global.r().unschedule_callback(handle);
        }
    }

    //FIXME: add support for XML encoding guess stuff using XML spec
    fn text_response(&self) -> String {
        let encoding = self.final_charset().unwrap_or(UTF_8);

        // According to Simon, decode() should never return an error, so unwrap()ing
        // the result should be fine. XXXManishearth have a closer look at this later
        encoding.decode(&self.response.borrow(), DecoderTrap::Replace).unwrap().to_owned()
    }

    fn document_response(&self) -> Option<Root<Document>> {
        let mime_type = self.final_mime_type();
        //TODO: prescan the response to determine encoding if final charset is null
        let charset = self.final_charset().unwrap_or(UTF_8);
        let temp_doc: Root<Document>;
        match mime_type {
            Some(Mime(mime::TopLevel::Text, mime::SubLevel::Html, _)) => {
                if self.response_type.get() == XMLHttpRequestResponseType::_empty {
                    return None;
                }
                else {
                    temp_doc = self.document_text_html();
                }
            },
            Some(Mime(mime::TopLevel::Text, mime::SubLevel::Xml, _)) |
            Some(Mime(mime::TopLevel::Application, mime::SubLevel::Xml, _)) |
            None => {
                temp_doc = self.handle_xml();
            },
            Some(Mime(_, mime::SubLevel::Ext(sub), _)) => {
                if sub.ends_with("+xml") {
                    temp_doc = self.handle_xml();
                }
                else {
                    return None;
                }
            },
            _ => { return None; }
        }
        temp_doc.set_encoding_name(DOMString::from(charset.name()));
        Some(temp_doc)
    }

    fn document_text_html(&self) -> Root<Document>{
        let charset = self.final_charset().unwrap_or(UTF_8);
        let wr = self.global();
        let wr = wr.r();
        let decoded = charset.decode(&self.response.borrow(), DecoderTrap::Replace).unwrap().to_owned();
        let document = self.new_doc(IsHTMLDocument::HTMLDocument);
        // TODO: Disable scripting while parsing
        parse_html(document.r(), DOMString::from(decoded), wr.get_url(), ParseContext::Owner(Some(wr.pipeline())));
        document
    }

    fn handle_xml(&self) -> Root<Document> {
        let charset = self.final_charset().unwrap_or(UTF_8);
        let wr = self.global();
        let wr = wr.r();
        let decoded = charset.decode(&self.response.borrow(), DecoderTrap::Replace).unwrap().to_owned();
        let document = self.new_doc(IsHTMLDocument::NonHTMLDocument);
        // TODO: Disable scripting while parsing
        parse_xml(document.r(), DOMString::from(decoded), wr.get_url(), xml::ParseContext::Owner(Some(wr.pipeline())));
        document
    }

    fn new_doc(&self, is_html_document: IsHTMLDocument) -> Root<Document> {
        let wr = self.global();
        let wr = wr.r();
        let win = wr.as_window();
        let doc = win.Document();
        let doc = doc.r();
        let docloader = DocumentLoader::new(&*doc.loader());
        let base = self.global().r().get_url();
        let parsed_url = match base.join(&self.ResponseURL().0) {
            Ok(parsed) => Some(parsed),
            Err(_) => None // Step 7
        };
        let mime_type = self.final_mime_type();
        let content_type = mime_type.map(|mime|{
            DOMString::from(format!("{}", mime))
        });
        Document::new(win,
                      parsed_url,
                      is_html_document,
                      content_type,
                      None,
                      DocumentSource::FromParser, docloader)
    }

    fn filter_response_headers(&self) -> Headers {
        // https://fetch.spec.whatwg.org/#concept-response-header-list
        use hyper::error::Result;
        use hyper::header::SetCookie;
        use hyper::header::{Header, HeaderFormat};
        use std::fmt;

        // a dummy header so we can use headers.remove::<SetCookie2>()
        #[derive(Clone, Debug, HeapSizeOf)]
        struct SetCookie2;
        impl Header for SetCookie2 {
            fn header_name() -> &'static str {
                "set-cookie2"
            }

            fn parse_header(_: &[Vec<u8>]) -> Result<SetCookie2> {
                unimplemented!()
            }
        }
        impl HeaderFormat for SetCookie2 {
            fn fmt_header(&self, _f: &mut fmt::Formatter) -> fmt::Result {
                unimplemented!()
            }
        }

        let mut headers = self.response_headers.borrow().clone();
        headers.remove::<SetCookie>();
        headers.remove::<SetCookie2>();
        // XXXManishearth additional CORS filtering goes here
        headers
    }

    fn discard_subsequent_responses(&self) {
        self.response_status.set(Err(()));
    }

    fn fetch(&self,
              load_data: LoadData,
              cors_request: Result<Option<CORSRequest>, ()>,
              global: GlobalRef) -> ErrorResult {
        let cors_request = match cors_request {
            Err(_) => {
                // Happens in case of cross-origin non-http URIs
                self.process_partial_response(XHRProgress::Errored(
                    self.generation_id.get(), Error::Network));
                return Err(Error::Network);
            }
            Ok(req) => req,
        };

        let xhr = Trusted::new(self, global.networking_thread_source());

        let context = Arc::new(Mutex::new(XHRContext {
            xhr: xhr,
            cors_request: cors_request.clone(),
            gen_id: self.generation_id.get(),
            buf: DOMRefCell::new(vec!()),
            sync_status: DOMRefCell::new(None),
        }));

        let (script_chan, script_port) = if self.sync.get() {
            let (tx, rx) = global.new_script_pair();
            (tx, Some(rx))
        } else {
            (global.networking_thread_source(), None)
        };

        let resource_thread = global.resource_thread();
        if let Some(req) = cors_request {
            XMLHttpRequest::check_cors(context.clone(), load_data, req.clone(),
                                       script_chan.clone(), resource_thread);
        } else {
            XMLHttpRequest::initiate_async_xhr(context.clone(), script_chan,
                                               resource_thread, load_data);
        }

        if let Some(script_port) = script_port {
            loop {
                global.process_event(script_port.recv());
                let context = context.lock().unwrap();
                let sync_status = context.sync_status.borrow();
                if let Some(ref status) = *sync_status {
                    return status.clone();
                }
            }
        }
        Ok(())
    }

    fn final_charset(&self) -> Option<EncodingRef> {
        if self.override_charset.borrow().is_some() {
            self.override_charset.borrow().clone()
        } else {
            match self.response_headers.borrow().get() {
                Some(&ContentType(ref mime)) => {
                    let value = mime.get_param(mime::Attr::Charset);
                    value.and_then(|value|{
                        encoding_from_whatwg_label(value)
                    })
                }
                None => { None }
            }
        }
    }

    fn final_mime_type(&self) -> Option<Mime> {
        if self.override_mime_type.borrow().is_some() {
            self.override_mime_type.borrow().clone()
        } else {
            match self.response_headers.borrow().get() {
                Some(&ContentType(ref mime)) => { Some(mime.clone()) },
                None => { None }
            }
        }
    }
}

trait Extractable {
    fn extract(&self) -> (Vec<u8>, Option<DOMString>);
}
impl Extractable for SendParam {
    // https://fetch.spec.whatwg.org/#concept-bodyinit-extract
    fn extract(&self) -> (Vec<u8>, Option<DOMString>) {
        match *self {
            BlobOrStringOrURLSearchParams::String(ref s) => {
                let encoding = UTF_8 as EncodingRef;
                (encoding.encode(s, EncoderTrap::Replace).unwrap(),
                    Some(DOMString::from("text/plain;charset=UTF-8")))
            },
            BlobOrStringOrURLSearchParams::URLSearchParams(ref usp) => {
                // Default encoding is UTF-8.
                (usp.serialize(None).into_bytes(),
                    Some(DOMString::from("application/x-www-form-urlencoded;charset=UTF-8")))
            },
            BlobOrStringOrURLSearchParams::Blob(ref b) => {
                let data = b.get_data();
                let content_type = if b.Type().as_ref().is_empty() {
                    None
                } else {
                    Some(b.Type())
                };
                (data.get_bytes().to_vec(), content_type)
            },
        }
    }
}
