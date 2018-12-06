import i18n from 'i18next';

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
                tooBigFile: 'file is too big, max size is 3MB',
            },
            header: {
                settings: 'Settings',
                logout: 'Logout',
                register: 'Register',
                login: 'Login',
                Username: 'Username',

            },
            library: {
                sortBy: 'Sort By',
                popularityAscending: 'popularity ascending',
                popularityDescending: 'popularity descending',
                ratingAscending: 'rating ascending',
                ratingDescending: 'rating descending',
                releaseDateAscending: 'release date ascending',
                releaseDateDescending: 'release date descending',
                revenueAscending: 'revenue ascending',
                revenueDescending: 'revenue descending',
                originalTitleAscending: 'original title ascending',
                originalTitleDescending: 'original title descending',
                genres: 'Genres',
                releaseYear: 'Release year',
                any: 'Any',
                search: 'Search',
                close: 'Close',
                rating: 'Rating',
                noRating: 'No rating',
                action: 'Action',
                adventure: 'Adventure',
                animation: 'animation',
                comedy: 'comedy',
                crime: 'crime',
                documentary: 'documentary',
                drama: 'drama:',
                family: 'family',
                fantasy: 'fantasy',
                history: 'history',
                horror: 'Horror',
                music: 'music',
                mystery: 'mystery',
                romance: 'romance',
                scienceFiction: 'scienceFiction',
                tvMovie: 'tvMovie',
                thriller: 'thriller',
                war: 'war:',
                western: 'western',
                releaseDataUnknown: 'Release date unknown',
                loadMore: 'Load more'
            },
            movie: {
                unavailable: 'This movie is unavailable',
                stream: 'STREAM IN',
                cancel: 'Cancel',
                send: 'Send',
                originalTitle: 'Original title: ',
                releaseDate: 'Release date: ',
                runtime: 'Runtime: ',
                rating: 'Rating: ',
                liveComment: 'Leave a comment',
                directors: 'Directors: ',
                producers: 'Producers: ',
                mainCast: 'Main cast: ',
                streamIn: 'Stream in',
                minutes: 'minutes'
            },
            login: {
                LogIn: 'Log in',
                logIn: 'Log in',
                username: 'Username',
                password: 'Password',
                forgotPassword: 'Frogot password?',
            },
            registration: {
                registration: 'Registration',
                checkEmail: 'Please, check your email and follow the link we\'\ve sent to activate your account.',
                ok: 'ok',
                register: 'Register',
                fname: 'First Name',
                lname: 'Last Name',
                username: 'Username',
                email: 'Email',
                password: 'Password',
                confirmPassword: 'Confirm Password',
            }
        },
        ru: {
            settingsPage: {
                other: 'Другое',
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
                none: 'Отсутствует',
                profile: 'Профиль',
                changeField: 'Изменить поле',
                email: 'Почта',
                emailAddress: 'Почтовый адрес',
                emailDialog: 'Новий почтовий адрес будет сохранен только когда вы подтвердите его перейдя по ссылке которую мы туда отправили',
                ok: 'ок',
                avatar: 'Аватар',
                importPicture: 'Импортировать изображение из',
                uploadPicture: 'Загрузить новое изображение',
                deletePicture: 'Удалить текущее изображение',
                imageProcessingFailed: 'Обработка изображения не удалась, пожалуйста, попробуйте снова',
                tooBigFile: 'Файл слишком большой, максимальный размер - 3MB'
            },
            header: {
                settings: 'Настройки',
                logout: 'Выход',
                register: 'Регистрация',
                login: 'Вход'
            },
            library: {
                sortBy: 'Сортировать по',
                popularityAscending: 'Возрастанию популярности',
                popularityDescending: 'Убыванию популярности',
                ratingAscending: 'Возрастанию рейтинга',
                ratingDescending: 'Убыванию рейтинга',
                releaseDateAscending: 'Дате выхода, новые в начале',
                releaseDateDescending: 'Дате выхода, новые в конце',
                revenueAscending: 'Возрастанию сборов',
                revenueDescending: 'Убыванию сборов',
                originalTitleAscending: 'Алфавиту (А-Я)',
                originalTitleDescending: 'Алфавиту (Я-А)',
                genres: 'Жанры',
                releaseYear: 'Год выхода',
                any: 'Любой',
                search: 'Поиск',
                close: 'Закрыть',
                rating: 'Рейтинг',
                noRating: 'Рейтинг отсутствует',
                action: 'Экшн',
                adventure: 'Приключения',
                animation: 'Анимации',
                comedy: 'Комедия',
                crime: 'Криминал',
                documentary: 'Документальные',
                drama: 'Драма',
                family: 'Семейные',
                fantasy: 'Фантастика',
                history: 'История',
                horror: 'Ужасы',
                music: 'Мюзиклы',
                mystery: 'Мистика',
                romance: 'Романы',
                scienceFiction: 'Научная фантастика',
                tvMovie: 'Телесериалы',
                thriller: 'Триллер',
                war: 'Военные',
                western: 'Вестерн',
                releaseDataUnknown: 'Дата выхода неизвестна',
                loadMore: 'Показать больше'
            },
            movie: {
                unavailable: 'Этот фильм недоступен',
                stream: 'Смотреть в ',
                cancel: 'Отменить',
                send: 'Отправить',
                originalTitle: 'Оригинальное название: ',
                releaseDate: 'Дата выхода: ',
                runtime: 'Продолжительность : ',
                rating: 'Рейтинг: ',
                liveComment: 'Оставить коментарий',
                directors: 'Режиссеры: ',
                producers: 'Продюсеры: ',
                mainCast: 'В главных ролях: ',
                streamIn: 'Смотреть в',
                minutes: 'минут'
            },
            login: {
                LogIn: 'Вход',
                logIn: 'Войти',
                username: 'Логин',
                password: 'Пароль',
                forgotPassword: 'Забыли пароль?',
            },
            registration: {
                registration: 'Регистрация',
                checkEmail: 'Пожалуйста, проверьте свою почту и следуйте по ссылке которую мы отправили для активации Вашего аккаунта',
                ok: 'ок',
                register: 'Зарегистрироватся',
                fname: 'Имя',
                lname: 'Фамилия',
                username: 'Логин',
                email: 'Електронная почта',
                password: 'Пароль',
                confirmPassword: 'Подтверждение пароля',

            }
        }
    }
});

export default i18n;