"use strict";

exports.getLocalStorage = function() {
  return window.localStorage;
}

exports.getSessionStorage = function() {
  return window.sessionStorage;
}

exports.newMockStorage = function() {
  var storage = {};
  return {
    get length() {
      return Object.keys(storage).length;
    },
    key: function(index) {
      return Object.keys(storage)[index];
    },
    getItem: function(key) {
      return storage[key];
    },
    setItem: function(key, item) {
      storage[key] = item;
    },
    removeItem: function(key) {
      delete storage[key];
    },
    clear: function() {
      Object.keys(storage).forEach(this.removeItem, this);
    }
  };
};
