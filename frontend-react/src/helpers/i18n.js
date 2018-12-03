import i18n from 'i18next';
import {DialogTitle} from "@material-ui/core";
import React from "react";

i18n.init({
    debug: true,

    lng: 'en',
    resources: {
        en: {
            settingsPage: {
                other: 'Other',
                language: 'Language',
                cancel: 'Cancel',
                save: 'Save',
                account: 'Account',
                changePassword: 'Change password',
                currentPassword: 'Current password',
                newPassword: 'New password',
                confirmPassword: 'Confirm new password',
                changeEmail: 'Change Email',
                fname: 'First Name',
                lname: 'Last Name',
                uname: 'Username',
                bio: 'Bio',
                none: 'None',
                profile: 'Profile',
                changeField: 'Change',
                email: 'Email',
                emailAddress: 'Email address',
                emailDialog: 'New email address will be saved only if you confirmed it by following our confirmation link.',
                ok: 'ok',
                avatar: 'Avatar',
                importPicture: 'Import picture from',
                uploadPicture: 'Upload new picture',
                deletePicture: 'Delete current picture',
                imageProcessingFailed: 'image processing failed, please try again',
                tooBigFile: 'file is too big, max size is 3MB'
            },
            header: {
                settings: 'Settings',
                logout: 'Logout',
                register: 'Register',
                login: 'Login'
            }
        },
        ru: {
            settingsPage: {
                other: 'Другие',
                language: 'Язык',
                cancel: 'Закрыть',
                save: 'Сохранить',
                account: 'Аккаунт',
                changePassword: 'Изменить пароль',
                currentPassword: 'Текущий пароль',
                newPassword: 'Новый пароль',
                confirmPassword: 'Подтвердите новый пароль',
                changeEmail: 'Изменить почту',
                fname: 'Имя',
                lname: 'Фамилия',
                uname: 'Логин',
                bio: 'Биография',
                none: '-',
                profile: 'Профиль',
                changeField: 'Изменить поле',
                email: 'Почта',
                emailAddress: 'Почтовый адрес',
                emailDialog: 'Новий почтовий адрес будет сохранен только если вы подтвердите его перейдя по ссылке в отпраленом письме',
                ok: 'оk',
                avatar: 'Аватар',
                importPicture: 'Импортировать изображение из',
                uploadPicture: 'Загрузить новое изображение',
                deletePicture: 'Удалить текущее изображение',
                imageProcessingFailed: 'Обработка изображения не удалась, пожалуйста, попробуйте снова',
                tooBigFile: 'Фаил слишком большой, максимальный размер - 3MB'
            },
            header: {
                settings: 'Настройки профиля',
                logout: 'Выход',
                register: 'Регистрация',
                login: 'Вход'
            }
        }
    }
});

export default i18n;