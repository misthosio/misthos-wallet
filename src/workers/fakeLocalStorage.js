var cache = {};

function getItem (key) {
  return cache[key]
};

function setItem (key, item) {
  cache[key] = item;
};

function removeItem (key) {
  cache[key] = null;
};

var localStorage = {
  getItem: getItem,
  setItem: setItem,
  removeItem: removeItem
};

exports.localStorage = localStorage;
