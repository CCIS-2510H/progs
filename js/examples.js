/*
 * This is a JavaScript Scratchpad.
 *
 * Enter some JavaScript, then Right Click or choose from the Execute Menu:
 * 1. Run to evaluate the selected text,
 * 2. Inspect to bring up an Object Inspector on the result, or,
 * 3. Display to insert the result in a comment after the selection.
 */

// console.log(3)

var myarray = [1,2,3,4];

// add7: Integer -> Integer
// add 7 to the given integer
function add7(n) { return n + 7; }

/*
class makeobj {
  int value;

  makeobj(val) {
    this.value = val;
  }

  m(n) { return n + 1; }

  subtract() { return this.value - 1 ; }
}
*/


function makeobj(val) {
    this.value = val;  
};

makeobj.prototype = 
   { m : function(n) {return n + 1;},
     subtract : function() {
        return this.value - 1;
     }};



var o = new makeobj(7);
var os = myarray.map(function (n) { return new makeobj(n); });

console.log(os[1].subtract());



