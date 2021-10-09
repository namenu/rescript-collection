// debug only
@warning("-32")
let toBinString = %raw(`
function (n) {
  return "0b" + n.toString(2).padStart(8, '0');
}
`)
