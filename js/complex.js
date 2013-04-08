/*
 * This is a JavaScript Scratchpad.
 *
 * Enter some JavaScript, then Right Click or choose from the Execute Menu:
 * 1. Run to evaluate the selected text,
 * 2. Inspect to bring up an Object Inspector on the result, or,
 * 3. Display to insert the result in a comment after the selection.
 */

// A Complex is new Complex(Number, Number)

// A Complex has
// real : Number
// imag : Number

// A Complex has
// add : Complex -> Complex
// sub : Complex -> Complex
// real : Number
// imag : Number

function Complex(real, imag) {
    this.real = real;
    this.imag = imag;
}

// add : Complex -> Complex
// add this to the given comp
Complex.prototype.add = function (comp) {
    return new Complex(this.real + comp.real,
                       this.imag + comp.imag);
}

// sub : Complex -> Complex
Complex.prototype.sub = function (comp) {
    return new Complex(this.real - comp.real,
                       this.imag - comp.imag);
}

// equals : Complex -> Boolean
Complex.prototype.equals = function (comp) {
    return 0 == comp.sub(this).real &&
           0 == this.sub(comp).imag;
}

var x = new Complex(3,4);
var y = new Complex(1,2);
var z = new Complex(2,2);

console.log(x)
console.log("=======")
console.log(y.add(z))
console.log(x.equals(y.add(z)));

var o = Object.create(Complex.prototype);
o.real = 3;
o.imag = 4;

console.log(x.equals(o))

console.log(o instanceof Complex)





