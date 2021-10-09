let e = HashSet.Int.empty

let ar = Belt.Array.range(0, 16)

let s1 = Belt.Array.reduce(ar, HashSet.Int.empty, (s, i) => HashSet.Int.set(s, i))

let s2 =
  s1
  ->HashSet.Int.remove(0)
  ->HashSet.Int.remove(1)
  ->HashSet.Int.remove(2)
  ->HashSet.Int.remove(3)
  ->HashSet.Int.remove(4)
  ->HashSet.Int.remove(5)
  ->HashSet.Int.remove(6)
  ->HashSet.Int.remove(7)
  ->HashSet.Int.remove(8)
  ->HashSet.Int.remove(9)

s2->HashSet.Int.log

// assert (s1->HashSet.Int.size == 1)

// let n = 100

// type action = Insert(int) | Remove(int)

// let actions = Belt.Array.makeBy(n, _ => {
//   let x = Js.Math.random_int(0, n / 2)
//   switch Js.Math.random_int(0, 2) {
//   | 0 => Insert(x)
//   | _ => Remove(x)
//   }
// })

// let init = (HashSet.Int.empty, Belt.Set.Int.empty)

// Belt.Array.reduce(actions, init, ((s1, s2), action) => {
//   switch action {
//   | Insert(x) =>
//     Js.log(j`Insert $x`)
//     //  Belt.Set.Int.size has serious performance problems here!
//     let len =
//       Belt.Set.Int.size(s2) + if Belt.Set.Int.get(s2, x)->Belt.Option.isNone {
//         1
//       } else {
//         0
//       }
//     let s1 = s1->HashSet.Int.set(x)
//     let s2 = s2->Belt.Set.Int.add(x)
//     assert (s1->HashSet.Int.size == len)
//     (s1, s2)
//   | Remove(x) =>
//     Js.log(j`Remove $x`)
//     let len =
//       Belt.Set.Int.size(s2) - if Belt.Set.Int.get(s2, x)->Belt.Option.isSome {
//         1
//       } else {
//         0
//       }
//     let s1 = s1->HashSet.Int.remove(x)
//     let s2 = s2->Belt.Set.Int.remove(x)
//     assert (s1->HashSet.Int.size == len)
//     (s1, s2)
//   }
// })->ignore
