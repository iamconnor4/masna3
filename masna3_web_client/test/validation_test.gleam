import gleam/option.{None, Some}
import gleeunit/should

import validation.{InvalidMimeType, InvalidUUID}

// MIME Type Tests

pub fn empty_mime_type_test() {
  validation.validate_mime_type("")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn no_slash_mime_type_test() {
  validation.validate_mime_type("abc")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn slash_only_mime_type_test() {
  validation.validate_mime_type("/")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn missing_subtype_mime_type_test() {
  validation.validate_mime_type("abc/")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn missing_type_mime_type_test() {
  validation.validate_mime_type("/efg")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn two_slashes_mime_type_test() {
  validation.validate_mime_type("abc/def/")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn multiple_slashes_mime_type_test() {
  validation.validate_mime_type("abc/def/ghi")
  |> should.equal(Some(InvalidMimeType("Invalid MIME type")))
}

pub fn valid_simple_mime_type_test() {
  validation.validate_mime_type("text/plain")
  |> should.equal(None)
}

pub fn valid_complex_mime_type_test() {
  validation.validate_mime_type("application/vnd.ms-fontobject")
  |> should.equal(None)
}

pub fn valid_plus_mime_type_test() {
  validation.validate_mime_type("application/epub+zip")
  |> should.equal(None)
}

// UUID Tests

pub fn empty_uuid_test() {
  validation.validate_uuid("")
  |> should.equal(Some(InvalidUUID("Invalid UUID")))
}

pub fn random_text_uuid_test() {
  validation.validate_uuid("abcdefghi")
  |> should.equal(Some(InvalidUUID("Invalid UUID")))
}

pub fn symbols_uuid_test() {
  validation.validate_uuid("+-#%^&")
  |> should.equal(Some(InvalidUUID("Invalid UUID")))
}

pub fn uuid_v1_test() {
  validation.validate_uuid("33a3ac56-02f4-11f1-8de9-0242ac120002")
  |> should.equal(Some(InvalidUUID("Must be a V7 UUID")))
}

pub fn uuid_v4_test() {
  validation.validate_uuid("b23a5a0c-053c-41e0-bbc9-1e7e178641a7")
  |> should.equal(Some(InvalidUUID("Must be a V7 UUID")))
}

pub fn uuid_nil_test() {
  validation.validate_uuid("00000000-0000-0000-0000-000000000000")
  |> should.equal(Some(InvalidUUID("Must be a V7 UUID")))
}

pub fn uuid_guid_test() {
  validation.validate_uuid("1aba7ddb-bd67-4eaf-8129-05191c9f4d52")
  |> should.equal(Some(InvalidUUID("Must be a V7 UUID")))
}

pub fn valid_uuid_v7_test() {
  validation.validate_uuid("019c3064-4ee1-761e-abe5-b4fe90adbd98")
  |> should.equal(None)
}
