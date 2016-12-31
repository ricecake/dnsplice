;(function(root, factory){
	if (typeof define === 'function' && define.amd) {
		define(['underscore'], factory);
	} else if (typeof exports === 'object') {
		module.exports = factory(require('underscore'));
	} else {
		root.KnotConn = factory(root._);
	}
}(this, function(_){
'use strict';


//var obj = { value: [], tree: {} };
//'a.b.c.d.e.f'.split('.').reduce(function (last, curr) {
//  console.log(last);
//  last.tree[curr] = { value: [], tree: {} };
//  return last.tree[curr];
//},obj);

/*
 * Need to write some logic to implement trie behavior for matching
 * Basically: '*' matches any one label; '#' matches any number of labels
 * when a message comes in, we traverse the tree, executing callbacks in depth first pre order
 * when we hit a star in the binding, we'll need to traverse the subtree with the next label popped off,
 * regardless of what it is.
 * When we hit a hash, we'll need to do that many times, untill we've exhausted the entire routing key,
 * and then execute any callbacks bound to the hash itself.
 */

var defaults = {
	url: '/ws/',
	traits: {},
	eventHandlers: {
		'ping': function() {
			this.send('pong', null);
		}
	},
	connector: function(options) {
		return new WebSocket(options.url);
	},
	onClose: function(){
		var object = this;
		setTimeout(function(){
			object.connect(object.options);
		}, 10000);
	}
};

var KnotConn = function (options) {
	options = _.extend({}, defaults, options);
	this.eventHandlers = { value: [], tree: {} };
	this.addEventHandlers(options.eventHandlers);
	options.url = url(options.url);
	if (options.connection) {
		this.connection = options.connection;
	} else {
		this.connect(options);
	}
	this.options = options;
	this.state = {};
	return this;
};

KnotConn.prototype.connect = function(options) {
	if (this.connection != undefined) {
		this.connection.close();
	}
	this.connection = options.connector(options);
	this.connection.onopen = options.onOpen.bind(this);
	this.connection.onmessage = this._messageHandler.bind(this);
	this.connection.onclose = options.onClose.bind(this);
	return this;
};

KnotConn.prototype.addEventHandlers = function(Handlers) {
	var handlers = this.eventHandlers;
	_.each(Handlers, function(callback, key) {
		var terminal = key.split('.').reduce(function (node, label) {
			if(!_.has(node.tree, label)) {
				node.tree[label] = { value: [], tree: {} };
			}
			return node.tree[label];
		}, handlers);
		terminal.value.push(callback);
	});
};

function matchingTrieNodes(Key, Trie) {
	var callbackList = [];
	Key.split('.').reduce(function (node, label, offset, array) {
		if (node === undefined) {
			return undefined;
		}
		if (offset === array.length-1 ) {
			if (_.has(node.tree, '#')) {
				callbackList = callbackList.concat(node.tree['#'].value);
			}
			if (_.has(node.tree, '*')) {
				callbackList = callbackList.concat(node.tree['*'].value);
			}
			if (_.has(node.tree, label)) {
				callbackList = callbackList.concat(node.tree[label].value);
			}
			return undefined;
		} else {
			if (_.has(node.tree, '#')) {
				for(var i=1; i<array.length;i++){
					var subKey = array.slice(offset+i).join('.');
					var newCallbacks = matchingTrieNodes(subKey, node.tree['#']);
					callbackList = callbackList.concat(newCallbacks);
				}
				callbackList = callbackList.concat(node.tree['#'].value);
			}
			if (_.has(node.tree, '*')) {
				var subKey = array.slice(offset+1).join('.');
				var newCallbacks = matchingTrieNodes(subKey, node.tree['*']);
				callbackList = callbackList.concat(newCallbacks);
			}
			return node.tree[label];
		}
	}, Trie);
	return callbackList;
}

KnotConn.prototype.trigger = function(key, content, decoded) {
	var callbacks = matchingTrieNodes(key, this.eventHandlers);
	var object = this;
	_.map(callbacks, function(callback) {
		callback.call(object, key, content, decoded);
	});
}

KnotConn.prototype._messageHandler = function(event) {
	var type, content;
	var decoded = JSON.parse(event.data);
	type = decoded.type;
	content = decoded.content;
	this.trigger(type, content, decoded);
}

KnotConn.prototype.send = function(key, content, extra) {
	var data = _.extend({}, this.traits, extra, {
		'type': key
	});
	if (content) {
		data.content = content;
	}
	var message = JSON.stringify(data);
	this.connection.send(message);
	return this;
}

function url(s) {
	var l = window.location;
	return ((l.protocol === "https:") ? "wss://" : "ws://") + l.hostname + (((l.port != 80) && (l.port != 443)) ? ":" + l.port : "") + s;
}

return KnotConn;
}));
