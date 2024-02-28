// const image = document.querySelector('div[role="row"]').querySelector('img')
const title = document.querySelector('#app')

// const divs = document.querySelectorAll('div[role="row"]')
// const images = div.map((d) => {
// 	d.querySelector('img')
// })
const images = [...document.querySelectorAll('div[role="row"]')].map((d) =>
	console.log('img: ', d.querySelector('img'))
)

// `document.querySelector` may return null if the selector doesn't match anything.
if (images) {
	const text = [...images].map((x) => x.src())
	console.log('images: ', images)
	console.log('text: ', text)
	// const wordMatchRegExp = /[^\s]+/g // Regular expression
	// const words = text.matchAll(wordMatchRegExp)
	// matchAll returns an iterator, convert to array to get word count
	// const wordCount = [...words].length
	// const readingTime = Math.round(wordCount / 200)
	const badge = document.createElement('p')
	// Use the same styling as the publish information in an title's header
	badge.classList.add('color-secondary-text', 'type--caption')
	badge.textContent = text
}

title.addEventListener('click', async () => {
	const main = document.querySelector('div[id="main"]')
	const images = [...main.querySelectorAll('div[role="row"]')].map((d) => {
		// console.log('div: ', d)
		// console.log('img: ', d.querySelector('img')?.src)
		// console.log(
		// 	'img: ',
		// 	d.querySelectorAll('img')
		// 	// .querySelector("style[type='width: 100%;']")
		// )
		// console.log(
		// 	'imgs: ',
		// 	[...d.querySelectorAll('img')]?.[1]
		// 	// .querySelector("style[type='width: 100%;']")
		// )
		// console.log(
		// 	'srcs: ',
		// 	[...d.querySelectorAll('img')]?.[1]?.src
		// 	// .querySelector("style[type='width: 100%;']")
		// )
		return [...d.querySelectorAll('img')]?.[1]?.src
	})
	if (images) {
		console.log(
			'images: ',
			images.filter((x) => x !== undefined)
		)
	}
})

// .startsWith('blob')
// ? d.querySelector('img')?.src
// : ''
// hypotesis -> photos have the property: style="width: 100%;"
// i was getting all images, and it was not giving me the ones inside the div id=main
// now I am not getting the blob image, because it stops at the first, which is a "src="data:image/jpeg;base64,/" -> that is a thumbnail, very small, not what we want!
// the photos, when I query All have 2x images inside the div (3x if there is an emoji!)
// second level (div img) is the imag -> filter by blob
// third level (div img) is the emoji/vote)
