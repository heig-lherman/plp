// regex that ensures the first character is the same as the last character
// with a universe equal to {0,1}

const regex = /^([01])[01]*\1$/;
const correct = /^0|1|0[01]*0|1[01]1$/;
