const imageUrls = [
	'blob:https://web.whatsapp.com/fd7afb28-5988-49b1-a4cf-f52adbcd5869',
	'blob:https://web.whatsapp.com/47258bc2-f377-4981-8986-ee4164b3bb1c',
	'blob:https://web.whatsapp.com/452b59c5-3e23-4ec6-9b8a-08d45d4ea249',
	// Add more image URLs as needed
]

let currentIndex = 0
const imageContainer = document.getElementById('image-container')
const imageDisplay = document.getElementById('image-display')

function displayNextImage() {
	imageDisplay.src = imageUrls[currentIndex]
	imageContainer.style.opacity = 1 // Make the image visible
	currentIndex = (currentIndex + 1) % imageUrls.length // Move to the next image, looping back to the start if at the end
}

function hideImage() {
	imageContainer.style.opacity = 0 // Fade the image out before changing the source
}

// Change the image every 5 seconds
setInterval(() => {
	hideImage()
	setTimeout(displayNextImage, 1000) // Change the image after it fades out
}, 5000)

// Initially display the first image
displayNextImage()
