// const title = document.querySelector('#app')
// title.addEventListener('click', async () => {
// 	const main = document.querySelector('div[id="main"]')
// 	const objects = [...main.querySelectorAll('div[role="row"]')]
// 		.map((x) => {
// 			return x.firstChild
// 		})
// 		.map((div) => {
// 			const image = div?.getElementsByTagName('div')[1]
// 			const reaction = div?.querySelector('div').lastChild.firstChild
// 				? div
// 						?.querySelector('div')
// 						.lastChild.firstChild?.querySelector('span')?.innerText
// 					? div
// 							?.querySelector('div')
// 							.lastChild.firstChild?.querySelector('span')
// 							?.innerText
// 					: 1
// 				: 0

// 			return {
// 				source: [...image.querySelectorAll('img')]?.[1]?.src,
// 				reaction,
// 			}
// 		})
// 		.filter((o) => o['source'] !== undefined)

// 	if (objects) {
// 		console.log('objects: ', objects)
// 	}
// })

//  try to track changes in realtime
const observer = new MutationObserver((mutations) => {
	mutations.forEach((mutation) => {
		// console.log('mutation: ', mutation)
		if (
			mutation.type === 'childList' &&
			mutation.target?.firstChild &&
			[...mutation.target?.firstChild?.getElementsByTagName('img')]
				.length > 0
		) {
			// Send the object to the background script
			chrome.runtime.sendMessage({
				node: mutation.target?.offsetParent?.offsetParent,
			})
		} else if (mutation.addedNodes.length) {
			mutation.addedNodes.forEach((node) => {
				if (
					node.nodeName === 'DIV' &&
					node.getAttribute('role') === 'row'
				) {
					if (node.getElementsByTagName('img').length > 0) {
						// Send the object to the background script
						chrome.runtime.sendMessage({node: node.firstChild})
					}
				}
			})
		}
	})
})

// Start observing for changes in the document's body, targeting added nodes
observer.observe(document.body, {
	childList: true, // Set to true to observe for the addition/removal of child nodes
	characterData: true, // Observe data changes in text nodes
	attributes: true, // Set to true to observe attribute changes
	subtree: true, // Set to true to observe descendants of the target node
})

console.log('WhatsApp Web content script loaded and monitoring for new images.')
