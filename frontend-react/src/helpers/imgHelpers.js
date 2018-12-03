import {Avatar} from "@material-ui/core";
import React from "react";

/**
 * @param {File} file - Image File Object
 */

const imgFileToBase64 = (file) => {
	return new Promise((resolve, reject) => {
		let reader = new FileReader();

		reader.onload = e => {
			resolve(e.target.result);
		}

		reader.onerror = e => {
			console.error(e.target.error);
			reject(e.target.error);
		}

		reader.readAsDataURL(file);
	});
}


/**
 * @param {File} image - Image File Object
 * @param {Object} crop - pixelCrop Object provided by react-image-crop
 */

const getCroppedImgBlob = (image, crop) => {
	const canvas = document.createElement('canvas');
	canvas.width = crop.width;
	canvas.height = crop.height;
	
	const context = canvas.getContext('2d');

	context.drawImage(
		image,
		crop.x,
		crop.y,
		crop.width,
		crop.height,
		0,
		0,
		crop.width,
		crop.height
	);

	return new Promise((resolve, reject) => {
		canvas.toBlob(file => {
			file.name = image.name;
			resolve(file);
		}, 'image/jpeg');
	});
};

const renderAvatar = (data, classes) => {
    if (data.avatar) {
        return <Avatar className={classes ? classes.avatar : ''} src={`http://localhost:8080${data.avatar}`} />;
    } else {
        return (
            <Avatar className={classes ? classes.avatar : ''} src={data.avatar} >
                {`${data.fname.charAt(0)}${data.lname.charAt(0)}`}
            </Avatar>
        );
    }
}

export default {
	imgFileToBase64,
	getCroppedImgBlob,
    renderAvatar
};
