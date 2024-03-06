// background.js
let socket = null
const serverUrl = 'ws://localhost:3001' // Update with your actual server WebSocket URL

function connectWebSocket() {
	socket = new WebSocket(serverUrl)

	socket.onopen = function () {
		console.log('WebSocket connection established with the server.')
	}

	socket.onerror = function (error) {
		console.error('WebSocket error:', error)
	}

	socket.onmessage = function (event) {
		// Handle messages received from the server
    console.log('Message from server:', event.data);

    // Assuming animation.html is open in a tab, find that tab
    chrome.tabs.query({url: chrome.runtime.getURL('animation.html')}, function(tabs) {
        if (tabs.length > 0) {
            // Assuming animation.html is only open in one tab
            let animationTabId = tabs[0].id;
            // Send the data to animation.js running in the tab with animation.html
            chrome.tabs.sendMessage(animationTabId, { action: "newImages", data: event.data });
        } else {
            console.log('animation.html is not open');
        }
    });
}

// Listen for messages from content scripts
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
		console.log('request: ', request)
	if (request.node) {
		console.log('Received object from content script:', request.node)
		if (socket && socket.readyState === WebSocket.OPEN) {
			// Send the image URL to the server
			socket.send(JSON.stringify({node: request.node}))
		} else {
			console.log('WebSocket not connected. Reconnecting...')
			connectWebSocket()
		}
	} else if (request.action === 'openAnimationPage') {
		chrome.tabs.create({url: chrome.runtime.getURL('animation.html')})
		sendResponse({status: 'Animation page opened'})
	}
})

// Ensure WebSocket connection is open
if (!socket || socket.readyState !== WebSocket.OPEN) {
	connectWebSocket()
}
