// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "@rescript/std/lib/es6/curry.js";
import * as Immutable from "immutable";
import * as Belt_Array from "@rescript/std/lib/es6/belt_Array.js";
import * as HashSet_Int from "../src/HashSet_Int.js";

function insertTest(param) {
  var ar = Belt_Array.range(0, 16);
  var s1 = Belt_Array.reduce(ar, HashSet_Int.empty, HashSet_Int.add);
  HashSet_Int.log(s1);
  if (HashSet_Int.size(s1) === 17) {
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "arrayMap_test.res",
          9,
          2
        ],
        Error: new Error()
      };
}

function removeTest(param) {
  var ar = Belt_Array.range(0, 16);
  var s1 = Belt_Array.reduce(ar, HashSet_Int.empty, HashSet_Int.add);
  return HashSet_Int.log(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(HashSet_Int.remove(s1, 0), 1), 2), 3), 4), 5), 6), 7), 8), 9));
}

function faillingTest(param) {
  var actions = [
    19,
    33,
    25,
    37,
    30,
    3,
    42,
    22,
    35,
    23,
    8,
    9,
    34,
    17,
    2,
    27,
    48,
    43,
    31,
    49,
    23
  ];
  var s = Belt_Array.reduce(actions, HashSet_Int.empty, HashSet_Int.add);
  if (HashSet_Int.size(s) === 20) {
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "arrayMap_test.res",
          42,
          2
        ],
        Error: new Error()
      };
}

faillingTest(undefined);

function perfComparison(rep) {
  var repeat = function (n, f) {
    for(var _for = 1; _for <= n; ++_for){
      Curry._1(f, undefined);
    }
    
  };
  var a1k = Belt_Array.makeByAndShuffle(1000, (function (i) {
          return i;
        }));
  console.time("arrayMap_test.res 55");
  var timed = repeat(rep, (function (param) {
          HashSet_Int.fromArray(a1k);
          
        }));
  console.log((console.timeEnd("arrayMap_test.res 55"), timed));
  console.time("arrayMap_test.res 57");
  var timed$1 = repeat(rep, (function (param) {
          new Immutable.Set(a1k);
          
        }));
  console.log((console.timeEnd("arrayMap_test.res 57"), timed$1));
  
}

perfComparison(1000);

var e = HashSet_Int.empty;

export {
  e ,
  insertTest ,
  removeTest ,
  faillingTest ,
  perfComparison ,
  
}
/*  Not a pure module */
