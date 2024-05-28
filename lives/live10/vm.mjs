#!/usr/bin/env node

import { readFile } from 'node:fs';

const [, , file] = process.argv;
const code = await readFile(file, 'utf-8');

const instrs = code.split('\n');

const stack = [];
const memory = {};

let pc = 0;
while (pc < instrs.length) {
    const instr = instrs[pc];
    const [op, ...args] = instr.split(' ');
    switch (op) {
        case 'PUSH':
            stack.push(args[0]);
            break;
        case 'POP':
            stack.pop();
            break;
        case 'WRITE':
            memory[args[0]] = stack.pop();
            break;
        case 'READ':
            stack.push(memory[args[0]]);
            break;
        case 'ADD': {
            const right = stack.pop();
            const left = stack.pop();
            stack.push(left + right);
            break;
        }
        case 'SUB': {
            const right = stack.pop();
            const left = stack.pop();
            stack.push(left - right);
            break;
        }
        case 'DISPLAY':
            console.log(stack.pop());
            break;
        case 'JUMP':
            pc += args[0];
            continue;
        case 'JUMPZERO':
            if (stack.pop() === 0) {
                pc += args[0];
                continue;
            }
            break;
        default:
            throw new Error(`Unknown instruction: ${op}`);
    }

    pc++;
}
