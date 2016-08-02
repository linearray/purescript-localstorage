"use strict";

// module Browser.LocalStorage.Raw

exports.localStorageImpl = function(/*eff*/) {
  return window.localStorage;
}

exports.sessionStorageImpl = function(/*eff*/) {
  return window.sessionStorage;
}

exports.mkStorageImpl = function(nothing) { return function(just) { return function (storage) {
  function toMaybe(value) { return value == null ? nothing : just(value); }
  function toUnit() { return {}; }
  return {
    length: function(/*eff*/) {
      return storage.length;
    },
    key: function(index) { return function(/*eff*/) {
      return toMaybe(storage.key(index));
    }; },
    getItem: function(key) { return function(/*eff*/) {
      return toMaybe(storage.getItem(key));
    }; },
    setItem: function(key) { return function(item) { return function (/*eff*/) {
      return toUnit(storage.setItem(key, item));
    }; }; },
    removeItem: function(key) { return function(/*eff*/) {
      return toUnit(storage.removeItem(key));
    }; },
    clear: function(/*eff*/) {
      return toUnit(storage.clear());
    },
  };
}; }; };
