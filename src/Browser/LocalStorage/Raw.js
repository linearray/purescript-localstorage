"use strict";

// module Browser.LocalStorage.Raw

exports.localStorageImpl = function(/*eff*/) {
  return window.localStorage;
}

exports.sessionStorageImpl = function(/*eff*/) {
  return window.sessionStorage;
}

exports.mockStorageImpl = function(/*eff*/){
  var storage = {};
  return {
    get keys() { return Object.keys(storage); },
    get length() { return this.keys.length; },
    key: function(index) { return this.keys[index]; },
    getItem: function(key) { return storage[key]; },
    setItem: function(key, item) { storage[key] = item; },
    removeItem: function(key) { delete storage[key]; },
    clear: function() { this.keys.forEach(this.removeItem, this); }
  };
};

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
