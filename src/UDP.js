'use strict';
var dgram = require('dgram')

// module Datagram.UDP

exports._createSocket = function(socketType) {
    return function() {
        return dgram.createSocket(socketType)
    }
}

exports._closeSocket = function(socket) {
    return function() {
        socket.close()
        return {}
    }
}

exports._onMessage = function(callback) {
    return function(socket) {
        return function() {
            socket.on('message', function(buf, rinfo) {
              callback(buf)(rinfo)()  
            })
            return {}
        }
    }
}

exports.bind = function(port) {
    return function(address) {
        return function(socket) {
            return function(success, error) {
                try {
                    socket.bind(port.value0, address.value0, function() {
                        success(socket.address())
                    })
                } catch(e) {
                    error(e)
                }
            }
        }
    }
}

exports._send = function(buffer) {
    return function(offset) {
        return function(length) {
            return function(port) {
                return function(address) {
                    return function(socket) {
                        return function() {
                            socket.send(buffer, offset, length, port, address)
                            return {}
                        }
                    }
                }
            }
        }
    }
}


exports._ref = function(socket) {
    return function() {
        return socket.ref()
    }
}

exports._unref = function(socket) {
    return function() {
        return socket.unref()
    }
}
