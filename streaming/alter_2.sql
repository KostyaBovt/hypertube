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