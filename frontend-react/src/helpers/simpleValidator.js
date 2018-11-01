import * as EmailValidator from 'email-validator';

export default (fieldName, value) => {
    const result = {
        isValid: true,
        error: ''
    }

    if (fieldName === "uname") {
        if (value.length < 6){
            result.error = "Username is too short.";
            result.isValid = false;
        } else if (value.length > 16) {
            result.error = "Username is too long";
            result.isValid = false;
        }
    } else if (fieldName === "fname"){
        if (value.length < 1){
            result.error = "First name is too short.";
            result.isValid = false;
        } else if (value.length > 50) {
            result.error = "First name is too long";
            result.isValid = false;
        }
    } else if (fieldName === "lname"){
        if (value.length < 1){
            result.error = "Last name is too short.";
            result.isValid = false;
        } else if (value.length > 50) {
           result.error = "Last name is too long";
           result.isValid = false;
        }
    } else if (fieldName === "bio" && value.length > 200) {
        result.error = "Bio is too long.";
        result.isValid = false;
    } else if (fieldName === "email" && !EmailValidator.validate(value)) {
        result.error = "Email is invalid.";
        result.isValid = false;
    }

    return result;
}