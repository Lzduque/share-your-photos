// const image = document.querySelector('div[role="row"]').querySelector('img')
const title = document.querySelector('#app')

// const divs = document.querySelectorAll('div[role="row"]')
// const images = div.map((d) => {
// 	d.querySelector('img')
// })
// const images = [...document.querySelectorAll('div[role="row"]')].map((d) =>
// 	console.log('img: ', d.querySelector('img'))
// )

// `document.querySelector` may return null if the selector doesn't match anything.
// if (images) {
// 	const text = [...images].map((x) => x.src())
// 	console.log('images: ', images)
// 	console.log('text: ', text)
// 	// const wordMatchRegExp = /[^\s]+/g // Regular expression
// 	// const words = text.matchAll(wordMatchRegExp)
// 	// matchAll returns an iterator, convert to array to get word count
// 	// const wordCount = [...words].length
// 	// const readingTime = Math.round(wordCount / 200)
// 	const badge = document.createElement('p')
// 	// Use the same styling as the publish information in an title's header
// 	badge.classList.add('color-secondary-text', 'type--caption')
// 	badge.textContent = text
// }

title.addEventListener('click', async () => {
	const main = document.querySelector('div[id="main"]')
	const objects = [...main.querySelectorAll('div[role="row"]')]
		.map((x) => {
			// console.log('x.firstChild: ', x.firstChild)
			return x.firstChild
		})
		// .filter((x) => x.length !== 0)
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

			console.log('image: ', image)
			console.log('reaction: ', div?.querySelector('div'))
			console.log('reaction: ', div?.querySelector('div').lastChild)
			console.log(
				'reaction: ',
				div?.querySelector('div').lastChild.firstChild
			) // if it has 0 votes, it will be null | if it has 1 vote, it will be a button
			console.log(
				'reaction: ',
				div
					?.querySelector('div')
					.lastChild.firstChild?.querySelector('span')?.innerText
			) // if it has more than 1 vote | if it has 0 votes and 1 vote, it will be undefined
			console.log('reaction: ', reaction)
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

// the photos, when I query All have 2x images inside the div (3x if there is an emoji!)
// second level (div img) is the imag -> filter by blob
// third level (div img) is the emoji/vote)
// now I have to figure out how to get the amount of votes for the reaction
