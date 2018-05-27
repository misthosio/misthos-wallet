var cache = {};

function getItem (key) {
  return cache[key]
};

function setItem (key, item) {
  cache[key] = item;
};

var localStorage = {
  getItem: getItem,
  setItem: setItem
};

exports.localStorage = localStorage;
