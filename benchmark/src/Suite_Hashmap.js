// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mori from "mori";
import * as Immutable from "immutable";
import * as Belt_Array from "@rescript/std/lib/es6/belt_Array.js";
import * as Belt_Option from "@rescript/std/lib/es6/belt_Option.js";
import * as Belt_SetInt from "@rescript/std/lib/es6/belt_SetInt.js";
import * as Caml_option from "@rescript/std/lib/es6/caml_option.js";
import * as HashSet_Int from "rescript-hashmap/src/HashSet_Int.js";

var a1k = Belt_Array.makeByAndShuffle(1000, (function (i) {
        return i;
      }));

var h1k = HashSet_Int.fromArray(a1k);

var b1k = Belt_SetInt.fromArray(a1k);

var i1k = new Immutable.Set(a1k);

var m1k = Mori.into(Mori.set([]), a1k);

var setup = "let a1k = A.makeByAndShuffle(1000, i => i)";

var benchmarks = [
  {
    name: "HashSet.Int.fromArray",
    code: "HashSet.Int.fromArray(a1k)",
    f: (function () {
        return HashSet_Int.fromArray(a1k);
      })
  },
  {
    name: "Belt.Set.Int.fromArray",
    code: "Belt.Set.Int.fromArray(a1k)",
    f: (function () {
        return Belt_SetInt.fromArray(a1k);
      })
  },
  {
    name: "ImmutableJs.Set.fromArray",
    code: "ImmutableJs.Set.fromArray(a1k)",
    f: (function () {
        return new Immutable.Set(a1k);
      })
  },
  {
    name: "Mori.into",
    code: "Mori.into(Mori.set([]), a1k)",
    f: (function () {
        return Mori.into(Mori.set([]), a1k);
      })
  }
];

var suite_name = "Create";

var suite = {
  name: suite_name,
  setup: setup,
  benchmarks: benchmarks
};

var Create = {
  setup: setup,
  benchmarks: benchmarks,
  suite: suite
};

var setup$1 = "let a1k = A.makeByAndShuffle(1000, i => i)";

var benchmarks$1 = [
  {
    name: "HashSet.Int.add",
    code: "a1k->A.reduce(HashSet.Int.empty, HashSet.Int.add)",
    f: (function () {
        return Belt_Array.reduce(a1k, HashSet_Int.empty, HashSet_Int.add);
      })
  },
  {
    name: "Belt.Set.Int.add",
    code: "a1k->A.reduce(Belt.Set.Int.empty, Belt.Set.Int.add)",
    f: (function () {
        return Belt_Array.reduce(a1k, undefined, Belt_SetInt.add);
      })
  },
  {
    name: "ImmutableJs.Set.add",
    code: "a1k->A.reduce(ImmutableJs.Set.make(), ImmutableJs.Set.add)",
    f: (function () {
        return Belt_Array.reduce(a1k, new Immutable.Set(), (function (prim0, prim1) {
                      return prim0.add(prim1);
                    }));
      })
  },
  {
    name: "Mori.conj",
    code: "a1k->A.reduce(Mori.set([]), Mori.conj)",
    f: (function () {
        return Belt_Array.reduce(a1k, Mori.set([]), (function (prim0, prim1) {
                      return Mori.conj(prim0, prim1);
                    }));
      })
  }
];

var suite_name$1 = "Insert";

var suite$1 = {
  name: suite_name$1,
  setup: setup$1,
  benchmarks: benchmarks$1
};

var Insert = {
  setup: setup$1,
  benchmarks: benchmarks$1,
  suite: suite$1
};

var setup$2 = "let a1k = A.makeByAndShuffle(1000, i => i)\nlet h1k = a1k->HashSet.Int.fromArray\nlet b1k = a1k->Belt.Set.Int.fromArray\nlet i1k = a1k->ImmutableJs.Set.fromArray\nlet m1k = a1k |> Mori.into(Mori.set([]))";

var benchmarks$2 = [
  {
    name: "HashSet.Int.get",
    code: "a1k\n->A.forEach(v => {\n  assert (HashSet.Int.get(h1k, v)->Belt.Option.isSome)\n  assert (HashSet.Int.get(h1k, v * -1 - 1)->Belt.Option.isNone)\n})",
    f: (function () {
        return Belt_Array.forEach(a1k, (function (v) {
                      if (!Belt_Option.isSome(HashSet_Int.get(h1k, v))) {
                        throw {
                              RE_EXN_ID: "Assert_failure",
                              _1: [
                                "Suite_Hashmap.res",
                                95,
                                10
                              ],
                              Error: new Error()
                            };
                      }
                      if (Belt_Option.isNone(HashSet_Int.get(h1k, Math.imul(v, -1) - 1 | 0))) {
                        return ;
                      }
                      throw {
                            RE_EXN_ID: "Assert_failure",
                            _1: [
                              "Suite_Hashmap.res",
                              96,
                              10
                            ],
                            Error: new Error()
                          };
                    }));
      })
  },
  {
    name: "Belt.Set.Int.get",
    code: "a1k\n->A.forEach(v => {\n  assert (Belt.Set.Int.get(b1k, v)->Belt.Option.isSome)\n  assert (Belt.Set.Int.get(b1k, v * -1 - 1)->Belt.Option.isNone)\n})",
    f: (function () {
        return Belt_Array.forEach(a1k, (function (v) {
                      if (!Belt_Option.isSome(Belt_SetInt.get(b1k, v))) {
                        throw {
                              RE_EXN_ID: "Assert_failure",
                              _1: [
                                "Suite_Hashmap.res",
                                111,
                                10
                              ],
                              Error: new Error()
                            };
                      }
                      if (Belt_Option.isNone(Belt_SetInt.get(b1k, Math.imul(v, -1) - 1 | 0))) {
                        return ;
                      }
                      throw {
                            RE_EXN_ID: "Assert_failure",
                            _1: [
                              "Suite_Hashmap.res",
                              112,
                              10
                            ],
                            Error: new Error()
                          };
                    }));
      })
  },
  {
    name: "ImmutableJs.Set.get",
    code: "a1k\n->A.forEach(v => {\n  assert (ImmutableJs.Set.get(i1k, v)->Belt.Option.isSome)\n  assert (ImmutableJs.Set.get(i1k, v * -1 - 1)->Belt.Option.isNone)\n})",
    f: (function () {
        return Belt_Array.forEach(a1k, (function (v) {
                      if (!Belt_Option.isSome(i1k.get(v))) {
                        throw {
                              RE_EXN_ID: "Assert_failure",
                              _1: [
                                "Suite_Hashmap.res",
                                127,
                                10
                              ],
                              Error: new Error()
                            };
                      }
                      if (Belt_Option.isNone(i1k.get(Math.imul(v, -1) - 1 | 0))) {
                        return ;
                      }
                      throw {
                            RE_EXN_ID: "Assert_failure",
                            _1: [
                              "Suite_Hashmap.res",
                              128,
                              10
                            ],
                            Error: new Error()
                          };
                    }));
      })
  },
  {
    name: "Mori.get",
    code: "a1k\n->A.forEach(v => {\n  assert (Mori.get(m1k, v)->Belt.Option.isSome)\n  assert (Mori.get(m1k, v * -1 - 1)->Belt.Option.isNone)\n})",
    f: (function () {
        return Belt_Array.forEach(a1k, (function (v) {
                      if (!Belt_Option.isSome(Caml_option.nullable_to_opt(Mori.get(m1k, v)))) {
                        throw {
                              RE_EXN_ID: "Assert_failure",
                              _1: [
                                "Suite_Hashmap.res",
                                143,
                                10
                              ],
                              Error: new Error()
                            };
                      }
                      if (Belt_Option.isNone(Caml_option.nullable_to_opt(Mori.get(m1k, Math.imul(v, -1) - 1 | 0)))) {
                        return ;
                      }
                      throw {
                            RE_EXN_ID: "Assert_failure",
                            _1: [
                              "Suite_Hashmap.res",
                              144,
                              10
                            ],
                            Error: new Error()
                          };
                    }));
      })
  }
];

var suite_name$2 = "Access";

var suite$2 = {
  name: suite_name$2,
  setup: setup$2,
  benchmarks: benchmarks$2
};

var Access = {
  setup: setup$2,
  benchmarks: benchmarks$2,
  suite: suite$2
};

function map(x) {
  switch (x) {
    case /* Create */0 :
        return {
                suite: suite,
                url: "create"
              };
    case /* Insert */1 :
        return {
                suite: suite$1,
                url: "insert"
              };
    case /* Access */2 :
        return {
                suite: suite$2,
                url: "access"
              };
    
  }
}

function fromUrl(x) {
  switch (x) {
    case "access" :
        return /* Access */2;
    case "create" :
        return /* Create */0;
    case "insert" :
        return /* Insert */1;
    default:
      return ;
  }
}

var routes = [
  /* Create */0,
  /* Insert */1,
  /* Access */2
];

var Routes = {
  map: map,
  fromUrl: fromUrl,
  routes: routes
};

var A;

export {
  A ,
  a1k ,
  h1k ,
  b1k ,
  i1k ,
  m1k ,
  Create ,
  Insert ,
  Access ,
  Routes ,
  
}
/* a1k Not a pure module */
