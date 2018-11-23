--
-- PostgreSQL database dump
--

-- Dumped from database version 10.5 (Ubuntu 10.5-2.pgdg14.04+1)
-- Dumped by pg_dump version 10.5 (Ubuntu 10.5-2.pgdg14.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: Hypertube; Type: DATABASE; Schema: -; Owner: Hypertube
--

CREATE ROLE "Hypertube" WITH LOGIN SUPERUSER PASSWORD '12345';

CREATE DATABASE "Hypertube" WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'en_US.UTF-8' LC_CTYPE = 'en_US.UTF-8';


ALTER DATABASE "Hypertube" OWNER TO "Hypertube";

\connect "Hypertube"

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: comments; Type: TABLE; Schema: public; Owner: Hypertube
--

CREATE TABLE public.comments (
    id integer NOT NULL,
    user_id integer NOT NULL,
    imdb_id character varying NOT NULL,
    text character varying NOT NULL,
    dt timestamp without time zone NOT NULL
);


ALTER TABLE public.comments OWNER TO "Hypertube";

--
-- Name: comments_id_seq; Type: SEQUENCE; Schema: public; Owner: Hypertube
--

CREATE SEQUENCE public.comments_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comments_id_seq OWNER TO "Hypertube";

--
-- Name: comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: Hypertube
--

ALTER SEQUENCE public.comments_id_seq OWNED BY public.comments.id;



--
-- Name: users; Type: TABLE; Schema: public; Owner: Hypertube
--

CREATE TABLE public.users (
    id integer NOT NULL,
    uname character varying NOT NULL,
    fname character varying,
    lname character varying,
    bio character varying,
    email character varying,
    password character varying,
    locale character(2),
    avatar character varying,
    social_provider character varying,
    social_id character varying,
    social_token character varying
);


ALTER TABLE public.users OWNER TO "Hypertube";

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: Hypertube
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO "Hypertube";

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: Hypertube
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: comments id; Type: DEFAULT; Schema: public; Owner: Hypertube
--

ALTER TABLE ONLY public.comments ALTER COLUMN id SET DEFAULT nextval('public.comments_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: Hypertube
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: comments; Type: TABLE DATA; Schema: public; Owner: Hypertube
--

COPY public.comments (id, user_id, imdb_id, text, dt) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: Hypertube
--

COPY public.users (id, uname, fname, lname, bio, email, password, locale, avatar, social_provider, social_id, social_token) FROM stdin;
\.


--
-- Name: comments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: Hypertube
--

SELECT pg_catalog.setval('public.comments_id_seq', 1, false);


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: Hypertube
--

SELECT pg_catalog.setval('public.users_id_seq', 1, false);


--
-- Name: comments comments_pk; Type: CONSTRAINT; Schema: public; Owner: Hypertube
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pk PRIMARY KEY (id);



--
-- Name: users users_pk; Type: CONSTRAINT; Schema: public; Owner: Hypertube
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pk PRIMARY KEY (id);


--
-- Name: comments_imdb_id_idx; Type: INDEX; Schema: public; Owner: Hypertube
--

CREATE INDEX comments_imdb_id_idx ON public.comments USING btree (imdb_id);


--
-- Name: users_email_idx; Type: INDEX; Schema: public; Owner: Hypertube
--

CREATE UNIQUE INDEX users_email_idx ON public.users USING btree (email);


--
-- Name: users_social_provider_idx; Type: INDEX; Schema: public; Owner: Hypertube
--

CREATE UNIQUE INDEX users_social_provider_idx ON public.users USING btree (social_provider, social_id);


--
-- Name: users_uname_idx; Type: INDEX; Schema: public; Owner: Hypertube
--

CREATE UNIQUE INDEX users_uname_idx ON public.users USING btree (uname);


-- create history table

DROP TABLE IF EXISTS public.history;
CREATE TABLE public.history (
    user_id integer NOT NULL,
    film_id integer NOT NULL,
    imdb_id character varying NOT NULL,
    seen timestamp(0) DEFAULT now() NOT NULL
);

ALTER TABLE ONLY public.history
    ADD CONSTRAINT history_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id) ON UPDATE CASCADE ON DELETE CASCADE;





--
-- PostgreSQL database dump complete
--

