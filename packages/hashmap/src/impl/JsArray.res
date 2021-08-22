@val
external make: int => array<'a> = "Array"

external get: (array<'a>, int) => 'a = "%array_unsafe_get"
external set: (array<'a>, int, 'a) => unit = "%array_unsafe_set"

external length: array<'a> => int = "%array_length"

// Belt.Array.slice does element-wise operation for no reason (native support?)
// It's only better than Js.Array2.slice in its argument design.
let slice = (ar, ~offset, ~len) => Js.Array2.slice(ar, ~start=offset, ~end_=offset + len)

// Belt.Array.copy uses Belt.Array.slice internally.
// The fastest way to copy one array to another is to `slice``
@send
external clone: array<'a> => array<'a> = "slice"

// src and dst must not overlap
let blit = (~src, ~srcOffset, ~dst, ~dstOffset, ~len) =>
  for i in 0 to len - 1 {
    Js.Array2.unsafe_set(dst, dstOffset + i, get(src, srcOffset + i))
  }

let cloneAndSet = (ar, i, a) => {
  let newAr = clone(ar)
  set(newAr, i, a)
  newAr
}

let cloneWithout = (ar, i) => {
  let newAr = make(length(ar) - 1)
  blit(~src=ar, ~srcOffset=0, ~dst=newAr, ~dstOffset=0, ~len=i)
  blit(~src=ar, ~srcOffset=i + 1, ~dst=newAr, ~dstOffset=i, ~len=length(ar) - 1 - i)
  newAr
}

@send
external forEach: (array<'a>, 'a => unit) => unit = "forEach"