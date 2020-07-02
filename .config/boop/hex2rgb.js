/**
     {
         "api":1,
         "name":"HEX2RGB",
         "description":"Convert colors in hexadecimal to RGB.",
         "author":"Yohanes Bandung Bondowoso",
         "icon":"table",
         "tags":"flip"
     }
 **/

function main(input) {
  const items = input.text.split("\n");
  const convertedItems = items.map((item) => {
    if (item) {
      return makin(item);
    } else {
      return item;
    }
  });
  input.text = String(convertedItems.join("\n"));
}

function makin(input) {
  R = hexToR(input);
  G = hexToG(input);
  B = hexToB(input);

  return `rgb(${R.toString()}, ${G.toString()}, ${B.toString()})`;
}

function hexToR(h) {
  return parseInt(cutHex(h).substring(0, 2), 16);
}
function hexToG(h) {
  return parseInt(cutHex(h).substring(2, 4), 16);
}
function hexToB(h) {
  return parseInt(cutHex(h).substring(4, 6), 16);
}
function cutHex(h) {
  return h.charAt(0) == "#" ? h.substring(1, 7) : h;
}

