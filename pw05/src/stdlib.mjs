/**
 * The Z standard library
 */
const stdlib = {
    'stdlib.h': {
        'abort': { returnType: 'void', params: [] },
        'abs': { returnType: 'int', params: [{ name: 'x', type: 'int' }] },
        'calloc': { returnType: 'void*', params: [{ name: 'nmemb', type: 'int' }, { name: 'size', type: 'int' }] },
        'exit': { returnType: 'void', params: [{ name: 'status', type: 'int' }] },
        'free': { returnType: 'void', params: [{ name: 'ptr', type: 'void*' }] },
        'malloc': { returnType: 'void*', params: [{ name: 'size', type: 'int' }] },
        'realloc': { returnType: 'void*', params: [{ name: 'ptr', type: 'void*' }, { name: 'size', type: 'int' }] },
    },
    'stdio.h': {
        'getchar': { returnType: 'char', params: [] },
        'gets': { returnType: 'char*', params: [{ name: 's', type: 'char*' }] },
        'putchar': { returnType: 'int', params: [{ name: 'c', type: 'int' }] },
        'puts': { returnType: 'int', params: [{ name: 's', type: 'char*' }] },
    },
    'string.h': {
        'memcpy': { returnType: 'void*', params: [{ name: 'dest', type: 'void*' }, { name: 'src', type: 'void*' }, { name: 'n', type: 'int' }] },
        'memmove': { returnType: 'void*', params: [{ name: 'dest', type: 'void*' }, { name: 'src', type: 'void*' }, { name: 'n', type: 'int' }] },
        'memchr': { returnType: 'void*', params: [{ name: 's', type: 'void*' }, { name: 'c', type: 'int' }, { name: 'n', type: 'int' }] },
        'strcpy': { returnType: 'char*', params: [{ name: 'dest', type: 'char*' }, { name: 'src', type: 'char*' }] },
        'strncpy': { returnType: 'char*', params: [{ name: 'dest', type: 'char*' }, { name: 'src', type: 'char*' }, { name: 'n', type: 'int' }] },
        'strcat': { returnType: 'char*', params: [{ name: 'dest', type: 'char*' }, { name: 'src', type: 'char*' }] },
        'strncat': { returnType: 'char*', params: [{ name: 'dest', type: 'char*' }, { name: 'src', type: 'char*' }, { name: 'n', type: 'int' }] },
        'strcmp': { returnType: 'int', params: [{ name: 's1', type: 'char*' }, { name: 's2', type: 'char*' }] },
        'strncmp': { returnType: 'int', params: [{ name: 's1', type: 'char*' }, { name: 's2', type: 'char*' }, { name: 'n', type: 'int' }] },
        'strchr': { returnType: 'char*', params: [{ name: 's', type: 'char*' }, { name: 'c', type: 'int' }] },
        'strrchr': { returnType: 'char*', params: [{ name: 's', type: 'char*' }, { name: 'c', type: 'int' }] },
        'strstr': { returnType: 'char*', params: [{ name: 's1', type: 'char*' }, { name: 's2', type: 'char*' }] },
        'strlen': { returnType: 'int', params: [{ name: 's', type: 'char*' }] },
    },
};

export { stdlib };
