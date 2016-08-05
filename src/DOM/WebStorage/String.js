"use strict";

exports.lengthImpl = function(storage) {
  return storage.length;
};

exports.keyImpl = function(nothing, just, storage, index) {
  var key = storage.key(index);
  return key == null ? nothing : just(key);
};

exports.getItemImpl = function(nothing, just, storage, key) {
  var item = storage.getItem(key);
  return item == null ? nothing : just(item);
};

exports.setItemImpl = function(storage, key, item) {
  storage.setItem(key, item);
  return {};
};

exports.removeItemImpl = function(storage, key) {
  storage.removeItem(key);
  return {};
};

exports.clearImpl = function(storage) {
  storage.clear();
  return {};
};

exports.getItemRefImpl = function(storage, key, encode, decode, defaultItem) {
  return {
    get value() {
      return exports.getItemImpl(defaultItem, decode, storage, key);
    },
    set value(item) {
      exports.setItemImpl(storage, key, encode(item));
    },
  };
};
