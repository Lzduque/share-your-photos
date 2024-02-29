// const image = document.querySelector('div[role="row"]').querySelector('img')
const title = document.querySelector('#app')

title.addEventListener('click', async () => {
	const main = document.querySelector('div[id="main"]')
	const objects = [...main.querySelectorAll('div[role="row"]')]
		.map((x) => {
			return x.firstChild
		})
		.map((div) => {
			const image = div?.getElementsByTagName('div')[1] // always right... the source is always right in the end
			const reaction = div?.querySelector('div').lastChild.firstChild
				? div
						?.querySelector('div')
						.lastChild.firstChild?.querySelector('span')?.innerText
					? div
							?.querySelector('div')
							.lastChild.firstChild?.querySelector('span')
							?.innerText
					: 1
				: 0

			return {
				source: [...image.querySelectorAll('img')]?.[1]?.src,
				reaction,
			}
		})
		.filter((o) => o['source'] !== undefined)

	if (objects) {
		console.log('objects: ', objects)
	}
})

// now I have to be able to send this data to a back end
// https://62a5-45-72-223-164.ngrok-free.app

var channel
var socket
var handler = {
	onmessage: function (evt) {
		// evt.data will be what your server sends in channel.send_message
	},
}
var xhr = new XMLHttpRequest()
xhr.onReadyStateChange = function () {
	// error handling and whatnot elided
	if (xhr.readyState == 4 && xhr.status == 200) {
		// We got a response from the server. The responseText is
		// a channel token so we can listen for a "verified" message.
		token = xhr.responseText
		channel = new goog.appengine.Channel(token)
		socket = channel.open(handler)
	}
}
xhr.open('POST', 'https://62a5-45-72-223-164.ngrok-free.app', true)
xhr.send('HI')
