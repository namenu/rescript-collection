// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "@rescript/std/lib/es6/curry.js";
import * as Caml_option from "@rescript/std/lib/es6/caml_option.js";

function slice(ar, offset, len) {
  return ar.slice(offset, offset + len | 0);
}

function blit(src, srcOffset, dst, dstOffset, len) {
  for(var i = 0; i < len; ++i){
    dst[dstOffset + i | 0] = src[srcOffset + i | 0];
  }
  
}

function cloneNode(x) {
  if (x.TAG === /* Node */0) {
    return {
            TAG: /* Node */0,
            _0: x._0.slice()
          };
  }
  
}

function setNode(node, idx, v) {
  if (node.TAG === /* Node */0) {
    node._0[idx] = v;
    return ;
  }
  
}

function getNode(node, idx) {
  if (node.TAG === /* Node */0) {
    return node._0[idx];
  }
  
}

function make(param) {
  return {
          size: 0,
          shift: 5,
          root: {
            TAG: /* Node */0,
            _0: []
          },
          tail: []
        };
}

function length(v) {
  return v.size;
}

function tailOffset(param) {
  var size = param.size;
  if (size < 32) {
    return 0;
  } else {
    return (((size - 1 | 0) >>> 5) << 5);
  }
}

function newPath(_level, _node) {
  while(true) {
    var node = _node;
    var level = _level;
    if (level === 0) {
      return node;
    }
    _node = {
      TAG: /* Node */0,
      _0: [node]
    };
    _level = level - 5 | 0;
    continue ;
  };
}

function pushTail(size, level, parent, tail) {
  var ret = cloneNode(parent);
  var subIdx = ((size - 1 | 0) >>> level) & 31;
  if (level === 5) {
    setNode(ret, subIdx, tail);
    return ret;
  }
  if (parent.TAG !== /* Node */0) {
    return ;
  }
  var ar = parent._0;
  var newChild = subIdx < ar.length ? pushTail(size, level - 5 | 0, ar[subIdx], tail) : newPath(level - 5 | 0, tail);
  setNode(ret, subIdx, newChild);
  return ret;
}

function push(vec, x) {
  var tail = vec.tail;
  var root = vec.root;
  var shift = vec.shift;
  var size = vec.size;
  if (tail.length < 32) {
    var newTail = tail.slice();
    newTail[tail.length] = x;
    return {
            size: size + 1 | 0,
            shift: vec.shift,
            root: vec.root,
            tail: newTail
          };
  }
  var isRootOverflow = (size >>> 5) > (1 << shift);
  if (isRootOverflow) {
    var newRoot = {
      TAG: /* Node */0,
      _0: [
        root,
        newPath(shift, {
              TAG: /* Leaf */1,
              _0: tail
            })
      ]
    };
    return {
            size: size + 1 | 0,
            shift: vec.shift + 5 | 0,
            root: newRoot,
            tail: [x]
          };
  }
  var newRoot$1 = pushTail(size, shift, root, {
        TAG: /* Leaf */1,
        _0: tail
      });
  return {
          size: size + 1 | 0,
          shift: vec.shift,
          root: newRoot$1,
          tail: [x]
        };
}

function getArrayUnsafe(vec, idx) {
  if (idx >= tailOffset(vec)) {
    return vec.tail;
  }
  var node = vec.root;
  var level = vec.shift;
  while(level > 0) {
    var subIdx = (idx >>> level) & 31;
    node = getNode(node, subIdx);
    level = level - 5 | 0;
  };
  var ar = node;
  if (ar.TAG === /* Node */0) {
    return ;
  } else {
    return ar._0;
  }
}

function popTail(size, level, parent) {
  if (level <= 0) {
    return ;
  }
  var subIdx = ((size - 2 | 0) >>> level) & 31;
  if (parent.TAG !== /* Node */0) {
    return ;
  }
  var ar = parent._0;
  var child = popTail(size, level - 5 | 0, ar[subIdx]);
  if (child !== undefined) {
    var newAr = ar.slice();
    newAr[subIdx] = child;
    return {
            TAG: /* Node */0,
            _0: newAr
          };
  }
  if (subIdx === 0) {
    return ;
  }
  var newAr$1 = slice(ar, 0, ar.length - 1 | 0);
  return {
          TAG: /* Node */0,
          _0: newAr$1
        };
}

function pop(vec) {
  var tail = vec.tail;
  var shift = vec.shift;
  var size = vec.size;
  if (size <= 1) {
    return make(undefined);
  }
  if (tail.length > 1) {
    var newTail = slice(tail, 0, tail.length - 1 | 0);
    return {
            size: size - 1 | 0,
            shift: vec.shift,
            root: vec.root,
            tail: newTail
          };
  }
  var newTail$1 = getArrayUnsafe(vec, size - 2 | 0);
  var nr = popTail(size, shift, vec.root);
  var newRoot = nr !== undefined ? nr : ({
        TAG: /* Node */0,
        _0: []
      });
  if (newRoot.TAG !== /* Node */0) {
    return ;
  }
  var ar = newRoot._0;
  var isRootUnderflow = shift > 5 && ar.length === 1;
  if (isRootUnderflow) {
    return {
            size: size - 1 | 0,
            shift: shift - 5 | 0,
            root: ar[0],
            tail: newTail$1
          };
  } else {
    return {
            size: size - 1 | 0,
            shift: vec.shift,
            root: newRoot,
            tail: newTail$1
          };
  }
}

function getUnsafe(vec, i) {
  return getArrayUnsafe(vec, i)[i & 31];
}

function get(v, i) {
  if (i < 0 || i >= v.size) {
    return ;
  } else {
    return Caml_option.some(getUnsafe(v, i));
  }
}

function getExn(v, i) {
  if (!(i >= 0 && i < v.size)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Vector.res",
            227,
            2
          ],
          Error: new Error()
        };
  }
  return getUnsafe(v, i);
}

function updatedPath(node, level, i, x) {
  if (node.TAG === /* Node */0) {
    var ar = node._0;
    var subIdx = (i >>> level) & 31;
    var m = ar.slice();
    m[subIdx] = updatedPath(ar[subIdx], level - 5 | 0, i, x);
    return {
            TAG: /* Node */0,
            _0: m
          };
  }
  var m$1 = node._0.slice();
  m$1[i % 32] = x;
  return {
          TAG: /* Leaf */1,
          _0: m$1
        };
}

function setUnsafe(vec, i, x) {
  var offset = tailOffset(vec);
  if (i < offset) {
    return {
            size: vec.size,
            shift: vec.shift,
            root: updatedPath(vec.root, vec.shift, i, x),
            tail: vec.tail
          };
  }
  var newTail = vec.tail.slice();
  newTail[i & 31] = x;
  return {
          size: vec.size,
          shift: vec.shift,
          root: vec.root,
          tail: newTail
        };
}

function set(vec, i, x) {
  if (i < 0 || i >= vec.size) {
    return ;
  } else {
    return setUnsafe(vec, i, x);
  }
}

function setExn(vec, i, x) {
  if (!(i >= 0 && i < vec.size)) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Vector.res",
            262,
            2
          ],
          Error: new Error()
        };
  }
  return setUnsafe(vec, i, x);
}

function fromArray(ar) {
  var len = ar.length;
  if (len === 0) {
    return make(undefined);
  }
  var tailSize = (len & 31) === 0 ? 32 : len & 31;
  var tailOffset = len - tailSize | 0;
  var tail = slice(ar, tailOffset, tailSize);
  var i = 0;
  var init = make(undefined);
  var state = {
    size: tailSize,
    shift: init.shift,
    root: init.root,
    tail: tail
  };
  while(i < tailOffset) {
    var offset = i;
    var vec = state;
    var root = vec.root;
    var shift = vec.shift;
    var size = vec.size;
    var leaf = {
      TAG: /* Leaf */1,
      _0: slice(ar, offset, 32)
    };
    var isRootOverflow = offset === (1 << (shift + 5 | 0));
    var tmp;
    if (isRootOverflow) {
      var newRoot = {
        TAG: /* Node */0,
        _0: [
          root,
          newPath(shift, leaf)
        ]
      };
      tmp = {
        size: size + 32 | 0,
        shift: shift + 5 | 0,
        root: newRoot,
        tail: vec.tail
      };
    } else {
      var newRoot$1 = pushTail(offset + 1 | 0, shift, root, leaf);
      tmp = {
        size: size + 32 | 0,
        shift: vec.shift,
        root: newRoot$1,
        tail: vec.tail
      };
    }
    state = tmp;
    i = offset + 32 | 0;
  };
  return state;
}

function toArray(param) {
  var tail = param.tail;
  var data = Array(param.size);
  var idx = {
    contents: 0
  };
  var fromTree = function (node) {
    if (node.TAG === /* Node */0) {
      node._0.forEach(fromTree);
      return ;
    }
    var ar = node._0;
    var len = ar.length;
    blit(ar, 0, data, idx.contents, len);
    idx.contents = idx.contents + len | 0;
    
  };
  fromTree(param.root);
  blit(tail, 0, data, idx.contents, tail.length);
  return data;
}

function reduceU(vec, init, f) {
  var i = 0;
  var acc = init;
  while(i < vec.size) {
    var ar = getArrayUnsafe(vec, i);
    var len = ar.length;
    for(var j = 0; j < len; ++j){
      acc = f(acc, ar[j]);
    }
    i = i + len | 0;
  };
  return acc;
}

function reduce(vec, init, f) {
  return reduceU(vec, init, Curry.__2(f));
}

function mapU(vec, f) {
  return reduceU(vec, make(undefined), (function (res, v) {
                return push(res, f(v));
              }));
}

function map(vec, f) {
  return mapU(vec, Curry.__1(f));
}

function keepU(vec, f) {
  return reduceU(vec, make(undefined), (function (res, v) {
                if (f(v)) {
                  return push(res, v);
                } else {
                  return res;
                }
              }));
}

function keep(vec, f) {
  return keepU(vec, Curry.__1(f));
}

function keepMapU(vec, f) {
  return reduceU(vec, make(undefined), (function (acc, v) {
                var v$1 = f(v);
                if (v$1 !== undefined) {
                  return push(acc, Caml_option.valFromOption(v$1));
                } else {
                  return acc;
                }
              }));
}

function keepMap(vec, f) {
  return keepMapU(vec, Curry.__1(f));
}

function forEachU(vec, f) {
  var i = 0;
  while(i < vec.size) {
    var ar = getArrayUnsafe(vec, i);
    var len = ar.length;
    for(var j = 0; j < len; ++j){
      f(ar[j]);
    }
    i = i + len | 0;
  };
  
}

function forEach(vec, f) {
  return forEachU(vec, Curry.__1(f));
}

function someU(vec, f) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === vec.size) {
      return false;
    }
    if (f(getUnsafe(vec, i))) {
      return true;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function some(vec, f) {
  return someU(vec, Curry.__1(f));
}

function everyU(vec, f) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === vec.size) {
      return true;
    }
    if (!f(getUnsafe(vec, i))) {
      return false;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function every(vec, f) {
  return everyU(vec, Curry.__1(f));
}

export {
  make ,
  length ,
  push ,
  pop ,
  get ,
  getExn ,
  getUnsafe ,
  set ,
  setExn ,
  setUnsafe ,
  reduceU ,
  reduce ,
  mapU ,
  map ,
  keepU ,
  keep ,
  keepMapU ,
  keepMap ,
  forEachU ,
  forEach ,
  someU ,
  some ,
  everyU ,
  every ,
  fromArray ,
  toArray ,
  
}
/* No side effect */