// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Hash from "./Hash.mjs";
import * as JsArray from "./JsArray.mjs";
import * as Caml_obj from "@rescript/std/lib/es6/caml_obj.js";

function make(param) {
  return {
          bitmap: 0,
          data: []
        };
}

function ctpop(v) {
  var v$1 = v - ((v >>> 1) & 1431655765) | 0;
  var v$2 = (v$1 & 858993459) + ((v$1 >>> 2) & 858993459) | 0;
  var v$3 = v$2 + (v$2 >>> 4) & 252645135;
  return (Math.imul(v$3, 16843009) >>> 24);
}

function mask(hash, shift) {
  return (hash >>> shift) & 3;
}

function bitpos(hash, shift) {
  return (1 << mask(hash, shift));
}

function indexAtBitmapTrie(bitmap, bit) {
  return ctpop(bitmap & (bit - 1 | 0));
}

var toBinString = (function (n) { 
  return "0b" + n.toString(2).padStart(8, '0');
});

function find(_param, _shift, hash, key) {
  while(true) {
    var param = _param;
    var shift = _shift;
    var bitmap = param.bitmap;
    var bit = bitpos(hash, shift);
    var match = bitmap & bit;
    if (match === 0) {
      return ;
    }
    var idx = indexAtBitmapTrie(bitmap, bit);
    var child = param.data[idx];
    if (child.TAG !== /* SubTrie */0) {
      if (child._0 === key) {
        return child._1;
      } else {
        return ;
      }
    }
    _shift = shift + 2 | 0;
    _param = child._0;
    continue ;
  };
}

function assoc(self, shift, hash, key, value) {
  var data = self.data;
  var bitmap = self.bitmap;
  var bit = bitpos(hash, shift);
  var idx = indexAtBitmapTrie(bitmap, bit);
  var match = bitmap & bit;
  if (match !== 0) {
    var child = data[idx];
    if (child.TAG === /* SubTrie */0) {
      var newChild = {
        TAG: /* SubTrie */0,
        _0: assoc(child._0, shift + 2 | 0, hash, key, value)
      };
      return {
              bitmap: bitmap,
              data: JsArray.cloneAndSet(data, idx, newChild)
            };
    }
    var v = child._1;
    var k = child._0;
    if (k === key) {
      if (v === value) {
        return self;
      } else {
        return {
                bitmap: bitmap,
                data: JsArray.cloneAndSet(data, idx, {
                      TAG: /* MapEntry */1,
                      _0: k,
                      _1: v
                    })
              };
      }
    }
    var leaf = makeNode(shift, Hash.hash(k), k, v, hash, key, value);
    return {
            bitmap: bitmap,
            data: JsArray.cloneAndSet(data, idx, {
                  TAG: /* SubTrie */0,
                  _0: leaf
                })
          };
  }
  var n = ctpop(bitmap);
  var ar = Array(n + 1 | 0);
  JsArray.blit(data, 0, ar, 0, idx);
  ar[idx] = {
    TAG: /* MapEntry */1,
    _0: key,
    _1: value
  };
  JsArray.blit(data, idx, ar, idx + 1 | 0, n - idx | 0);
  return {
          bitmap: bitmap | bit,
          data: ar
        };
}

function makeNode(shift, h1, k1, v1, h2, k2, v2) {
  if (h1 === h2) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Hamt.res",
            156,
            2
          ],
          Error: new Error()
        };
  }
  return assoc(assoc({
                  bitmap: 0,
                  data: []
                }, shift + 2 | 0, h1, k1, v1), shift + 2 | 0, h2, k2, v2);
}

function dissoc(m, shift, hash, key) {
  var data = m.data;
  var bitmap = m.bitmap;
  var bit = bitpos(hash, shift);
  var match = bitmap & bit;
  if (match === 0) {
    return m;
  }
  var idx = indexAtBitmapTrie(bitmap, bit);
  var child = data[idx];
  if (child.TAG !== /* SubTrie */0) {
    if (child._0 === key) {
      if (bitmap === bit) {
        return ;
      } else {
        return {
                bitmap: bitmap ^ bit,
                data: JsArray.cloneWithout(data, idx)
              };
      }
    } else {
      return m;
    }
  }
  var trie = child._0;
  var newChild = dissoc(trie, shift + 2 | 0, hash, key);
  if (newChild !== undefined) {
    if (newChild === trie) {
      return m;
    } else {
      return {
              bitmap: bitmap,
              data: JsArray.cloneAndSet(data, idx, {
                    TAG: /* SubTrie */0,
                    _0: newChild
                  })
            };
    }
  } else if (bitmap === bit) {
    return ;
  } else {
    return {
            bitmap: bitmap ^ bit,
            data: JsArray.cloneWithout(data, idx)
          };
  }
}

function get(m, k) {
  return find(m, 0, Hash.hash(k), k);
}

function set(m, k, v) {
  return assoc(m, 0, Hash.hash(k), k, v);
}

function remove(m, k) {
  return dissoc(m, 0, Hash.hash(k), k);
}

var trie_data = [
  {
    TAG: /* MapEntry */1,
    _0: "Sir Robin",
    _1: 10
  },
  {
    TAG: /* MapEntry */1,
    _0: "Sir Bedevere",
    _1: 20
  }
];

var trie = {
  bitmap: 6,
  data: trie_data
};

var t2 = set(trie, "Sir Lancelot", 30);

var trie_data$1 = [
  {
    TAG: /* MapEntry */1,
    _0: "Sir Robin",
    _1: 10
  },
  {
    TAG: /* SubTrie */0,
    _0: {
      bitmap: 5,
      data: [
        {
          TAG: /* MapEntry */1,
          _0: "Sir Lancelot",
          _1: 30
        },
        {
          TAG: /* MapEntry */1,
          _0: "Sir Bedevere",
          _1: 20
        }
      ]
    }
  }
];

var trie$1 = {
  bitmap: 6,
  data: trie_data$1
};

if (!Caml_obj.caml_equal(t2, trie$1)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Hamt.res",
          258,
          0
        ],
        Error: new Error()
      };
}

var numBits = 2;

var maskBits = 3;

var A;

export {
  numBits ,
  maskBits ,
  make ,
  ctpop ,
  mask ,
  bitpos ,
  indexAtBitmapTrie ,
  toBinString ,
  find ,
  A ,
  assoc ,
  makeNode ,
  dissoc ,
  get ,
  set ,
  remove ,
  t2 ,
  trie$1 as trie,
  
}
/* t2 Not a pure module */
