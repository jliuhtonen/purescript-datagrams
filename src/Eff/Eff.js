'use strict';

var dgram = require('dgram')

// module Datagram.UDP.Eff

exports.createSocket = function(socketType) {
    return function() {
        return dgram.createSocket(socketType)
    }
}

exports.closeSocket = function(socket) {
    return function() {
        socket.close()
        return {}
    }
}

exports.bind = function(port) {
    return function(address) {
        return function(callback) {
            return function(socket) {
                return function() {
                    socket.bind(port, address, function() {
                        callback(socket.address())()
                    })
                }
            }
        }
    }
}

exports.send = function(socket) {
    return function(buffer) {
        return function(offset) {
            return function(length) {
                return function(port) {
                    return function(address) {
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

exports.ref = function(socket) {
    return function() {
        return socket.ref()
    }
}

exports.unref = function(socket) {
    return function() {
        return socket.unref()
    }
}
