create table countries
(
    id integer generated always as identity primary key,
    name text not null
);

create table users
(
    id integer generated always as identity primary key,
    name text not null,
    password text not null,
    joined timestamp default (now() at time zone 'utc'),
    email text not null,
    birthday date,
    gender text,
    headline text not null,
    avatar text,
    description text not null,
    country integer,
    active boolean not null default true,

    constraint country_user foreign key (country) references countries(id)
);

create table messages
(
    id integer generated always as identity primary key,
    temporary_id integer not null,
    sender integer not null,
    recipient integer not null,
    date timestamp not null default (now() at time zone 'utc'),
    content text not null,
    status smallint not null default 1,
    visualized timestamp,

    constraint from_user_message foreign key (sender) references users(id) on delete cascade,
    constraint to_user_message foreign key (recipient) references users(id) on delete cascade
);

create table tags
(
    id integer generated always as identity primary key,
    name text not null,
    constraint unique_tag unique(name)
);

create table languages
(
    id integer generated always as identity primary key,
    name text not null
);

create table blocks
(
    id integer generated always as identity primary key,
    blocker integer not null,
    blocked integer not null,
    constraint blocker_user foreign key (blocker) references users(id) on delete cascade,
    constraint blocked_user foreign key (blocked) references users(id) on delete cascade
);

create table tags_users
(
    id integer generated always as identity primary key,
    creator integer not null,
    tag integer not null,
    constraint tags_user_user foreign key (creator) references users(id) on delete cascade,
    constraint tag_user_tag foreign key (tag) references tags(id)
);

create table languages_users
(
    id integer generated always as identity primary key,
    speaker integer not null,
    language integer not null,
    constraint languages_user_user foreign key (speaker) references users(id) on delete cascade,
    constraint language_user_language foreign key (language) references languages(id),
    constraint unique_user_language unique(speaker, language)
);

create table recoveries
(
    id integer generated always as identity primary key,
    uuid char(36) not null,
    created timestamp default (now() at time zone 'utc'),
    active boolean default true,
    recoverer integer not null,
    constraint recoverer foreign key (recoverer) references users(id) on delete cascade
);

create table karma_histories
(
    id integer generated always as identity primary key,
    target integer not null,
    amount integer not null,
    date timestamp not null default (now() at time zone 'utc'),

    constraint target_karma_history foreign key (target) references users(id) on delete cascade
);

--to be run with pg cron
create or replace function crunch_karma_history(hours_time integer)
    returns void as
$$
begin
    create temporary table temp_karmas (id integer, target integer, amount integer ) on commit drop;
    insert into temp_karmas
    select id,
            target,
            amount
    from karma_histories
    where extract(epoch from (now() at time zone 'utc') - date ) / 3600 <= hours_time;
    delete from karma_histories k
    where exists(select t.id from temp_karmas t where t.id = k.id);
    insert into karma_histories(target, amount)
    select t.target,
            sum(t.amount)
    from temp_karmas t
    group by t.target;
end;
$$
language plpgsql;

-- select cron.schedule('0 * * * *', $$select crunch_karma_history(1)$$);
-- select cron.schedule('10 0 * * *', $$select crunch_karma_history(24)$$);
-- select cron.schedule('30 0 * * 1', $$select crunch_karma_history(24 * 7)$$);

create table karma_leaderboard
(
    id integer generated always as identity primary key,
    ranker integer not null,
    position integer not null,
    current_karma integer not null,
    gained integer not null,
    date timestamp not null default (now() at time zone 'utc'),

    constraint ranker_user foreign key (ranker) references users(id) on delete cascade
);

--to be run with pg cron
create or replace function compute_leaderboard()
    returns void as
$$
begin
    create temporary table temp_leaderboard (ranker integer, current_karma integer, position integer, gained integer) on commit drop;
    insert into temp_leaderboard(ranker, current_karma, position, gained)
    select  target,
            total,
            row_number() over (order by total desc),
            (select total - coalesce((select k.current_karma from karma_leaderboard k where k.ranker = target),0) )
    from (select target, sum(amount) total from karma_histories group by target) h;
    truncate table karma_leaderboard;
    insert into karma_leaderboard(ranker, current_karma, position, gained)
    select * from temp_leaderboard;
end;
$$
language plpgsql;

-- select cron.schedule('0 */4 * * *', $$select crunch_karma_history()$$);

create table histories
(
    id integer generated always as identity primary key,
    sender integer not null,
    recipient integer not null,
    first_message_date timestamp not null default (now() at time zone 'utc'),
    date @
    sender_archived boolean not null default false,
    recipient_archived boolean not null default false,

    constraint from_user_message foreign key (sender) references users(id) on delete cascade,
    constraint to_user_message foreign key (recipient) references users(id) on delete cascade,

    unique(sender, recipient)
);

create table suggestions
(
    id integer generated always as identity primary key,
    suggested integer not null,
    score integer not null,

    constraint user_suggested foreign key (suggested) references users(id) on delete cascade
);

create table reports
(
    id integer generated always as identity primary key,
    comment text,
    reason smallint not null,
    reporter integer not null,
    date timestamp not null default (now() at time zone 'utc'),
    reported integer not null,
    constraint reporter_user foreign key (reporter) references users(id) on delete cascade,
    constraint reported_user foreign key (reported) references users(id) on delete cascade
);

-- create table badges
-- (
--     id integer generated always as identity primary key,
--     kind smallint not null,
--     description text
-- );

-- create table reactions
-- (
--     id integer generated always as identity primary key,
--     reward integer not null,
--     kind smallint not null,
--     description text
-- );

-- create table privileges
-- (
--     id integer generated always as identity primary key,
--     feature smallint not null,
--     description text,
--     quantity integer not null
-- );

-- create table privileges_users
-- (
--     id integer generated always as identity primary key,
--     privilege integer not null,
--     receiver integer not null,
--     constraint privilege_user_user foreign key (receiver) references users(id) on delete cascade,
--     constraint privilege_user_privilege foreign key (privilege) references privileges(id) on delete cascade
-- );

-- create table badges_users
-- (
--     id integer generated always as identity primary key,
--     receiver integer not null,
--     badge integer not null,
--     constraint badge_user_user foreign key (receiver) references users(id) on delete cascade,
--     constraint badge_user_badge foreign key (badge) references badges(id)
-- );

-- create table reactions_messages
-- (
--     id integer generated always as identity primary key,
--     reaction integer not null,
--     message bigint not null,
--     constraint reaction_message_message foreign key (message) references messages(id) on delete cascade,
--     constraint reaction_message_reaction foreign key (reaction) references reactions(id) on delete cascade
-- );

-- create table reactions_users
-- (
--     id integer generated always as identity primary key,
--     bearer integer not null,
--     reaction integer not null,
--     constraint reaction_user_user foreign key (bearer) references users(id) on delete cascade,
--     constraint reaction_user_reaction foreign key (reaction) references reactions(id) on delete cascade
-- );

create table experiments
(
    id integer generated always as identity primary key,
    code integer not null,
    name text not null,
    description text not null,
    added timestamp not null default (now() at time zone 'utc')
);

create or replace function insert_history
(sender_id int, recipient_id int) returns void as
$$
begin
    if exists(select 1 from histories where sender = recipient_id and recipient = sender_id) then
        update histories set sender_archived = false, recipient_archived = false, date = (now() at time zone 'utc') where sender = recipient_id and recipient = sender_id;
    else
        insert into histories (sender, recipient)
        values (sender_id, recipient_id)
        on conflict (sender, recipient) do
        update set sender_archived = false, recipient_archived = false, date = (now() at time zone 'utc');
    end if;
end;
  $$
  language plpgsql;

-- create or replace function truncate_tables()
--   returns void as
-- $body$
-- begin
--         truncate table users  restart identity cascade;
--         truncate table messages restart identity cascade ;
--         truncate table tags restart identity cascade ;
-- end;
--   $body$
--   language plpgsql;

insert into languages
    (name)
values
    ('Albanian'),
    ('Amharic'),
    ('Arabic'),
    ('Arapaho'),
    ('Armenian'),
    ('Awadhi'),
    ('Azerbaijani'),
    ('Basque'),
    ('Bengali'),
    ('Bhojpuri'),
    ('Bosnian'),
    ('Breton'),
    ('Burmese'),
    ('Bulgarian'),
    ('Catalan'),
    ('Chinese-Gan'),
    ('Chinese-Hakka'),
    ('Chinese-Jinyu'),
    ('Chinese-Mandarin'),
    ('Chinese-MinNan'),
    ('Chinese-Wu'),
    ('Chinese-Xiang'),
    ('Chinese-Yue (Cantonese)'),
    ('Cornish'),
    ('Croatian'),
    ('Czech'),
    ('Danish'),
    ('Dutch'),
    ('English'),
    ('Esperanto'),
    ('Estonian'),
    ('Euchee/Yuchi'),
    ('Faroese'),
    ('Finnish'),
    ('French'),
    ('Galician'),
    ('German'),
    ('Georgian'),
    ('Greek'),
    ('Gujarati'),
    ('Haitian Creole'),
    ('Hausa'),
    ('Hebrew'),
    ('Hindi'),
    ('Hmong'),
    ('Hungarian'),
    ('Icelandic'),
    ('Igbo'),
    ('Indonesian'),
    ('Italian'),
    ('Irish Gaelic'),
    ('Japanese'),
    ('Javanese'),
    ('Kannada'),
    ('Khmer'),
    ('Konkani'),
    ('Korean'),
    ('Kurdish'),
    ('Latin'),
    ('Latvian'),
    ('Lithuanian'),
    ('Lojban'),
    ('Macedonian'),
    ('Maithili'),
    ('Malay'),
    ('Malayalam'),
    ('Manx'),
    ('Marathi'),
    ('Mongolian'),
    ('Mongul'),
    ('Navajo/Dine'),
    ('Norwegian'),
    ('Ojibwe/Anishnaabeg'),
    ('Oriya'),
    ('Panjabi-Eastern'),
    ('Panjabi-Western'),
    ('Papiamentu'),
    ('Pashto'),
    ('Persian'),
    ('Polish'),
    ('Portuguese'),
    ('Romanian'),
    ('Russian'),
    ('Sanskrit'),
    ('Sauk'),
    ('Scots Gaelic'),
    ('Serbian'),
    ('Serbo-Croatian'),
    ('Sindhi'),
    ('Sioux'),
    ('Slovak'),
    ('Slovenian'),
    ('Spanish'),
    ('Sunda'),
    ('Swahili'),
    ('Swedish'),
    ('Tagalog'),
    ('Tamazight'),
    ('Tamil'),
    ('Telugu'),
    ('Thai'),
    ('Turkish'),
    ('Twi'),
    ('Ukrainian'),
    ('Urdu'),
    ('Uzbek'),
    ('Vietnamese'),
    ('Wampanoag'),
    ('Welsh'),
    ('Wolof'),
    ('Yiddish'),
    ('Yoruba'),
    ('Zulu'),
    ('Other');

insert into countries
    (name)
values
    ('Afghanistan'),
    ('Albania'),
    ('Algeria'),
    ('Andorra'),
    ('Angola'),
    ('Antigua and Barbuda'),
    ('Argentina'),
    ('Armenia'),
    ('Australia'),
    ('Austria'),
    ('Azerbaijan'),
    ('Bahamas'),
    ('Bahrain'),
    ('Bangladesh'),
    ('Barbados'),
    ('Belarus'),
    ('Belgium'),
    ('Belize'),
    ('Benin'),
    ('Bhutan'),
    ('Bolivia'),
    ('Bosnia and Herzegovina'),
    ('Botswana'),
    ('Brazil'),
    ('Brunei Darussalam'),
    ('Bulgaria'),
    ('Burkina Faso'),
    ('Burma'),
    ('Burundi'),
    ('Cambodia'),
    ('Cameroon'),
    ('Canada'),
    ('Cape Verde'),
    ('Central African Republic'),
    ('Chad'),
    ('Chile'),
    ('China'),
    ('Colombia'),
    ('Comoros'),
    ('Congo (Brazzaville)'),
    ('Congo (Kinshasa)'),
    ('Costa Rica'),
    ('Cote d''Ivoire'),
    ('Croatia'),
    ('Cuba'),
    ('Cyprus'),
    ('Czech Republic'),
    ('Denmark'),
    ('Djibouti'),
    ('Dominica'),
    ('Dominican Republic'),
    ('East Timor'),
    ('Ecuador'),
    ('Egypt'),
    ('El Salvador'),
    ('Equatorial Guinea'),
    ('Eritrea'),
    ('Estonia'),
    ('Ethiopia'),
    ('Fiji'),
    ('Finland'),
    ('France'),
    ('Gabon'),
    ('Gambia'),
    ('Georgia'),
    ('Germany'),
    ('Ghana'),
    ('Greece'),
    ('Grenada'),
    ('Guatemala'),
    ('Guinea'),
    ('Guinea-Bissau'),
    ('Guyana'),
    ('Haiti'),
    ('Holy See'),
    ('Honduras'),
    ('Hong Kong'),
    ('Hungary'),
    ('Iceland'),
    ('India'),
    ('Indonesia'),
    ('Iran'),
    ('Iraq'),
    ('Ireland'),
    ('Israel'),
    ('Italy'),
    ('Jamaica'),
    ('Japan'),
    ('Jordan'),
    ('Kazakhstan'),
    ('Kenya'),
    ('Kiribati'),
    ('Kosovo'),
    ('Kuwait'),
    ('Kyrgyzstan'),
    ('Laos'),
    ('Latvia'),
    ('Lebanon'),
    ('Lesotho'),
    ('Liberia'),
    ('Libya'),
    ('Liechtenstein'),
    ('Lithuania'),
    ('Luxembourg'),
    ('Macau'),
    ('Macedonia'),
    ('Madagascar'),
    ('Malawi'),
    ('Malaysia'),
    ('Maldives'),
    ('Mali'),
    ('Malta'),
    ('Marshall Islands'),
    ('Mauritania'),
    ('Mauritius'),
    ('Mexico'),
    ('Micronesia'),
    ('Moldova'),
    ('Monaco'),
    ('Mongolia'),
    ('Montenegro'),
    ('Morocco'),
    ('Mozambique'),
    ('Namibia'),
    ('Nauru'),
    ('Nepal'),
    ('Netherlands'),
    ('Netherlands Antilles'),
    ('New Caledonia'),
    ('New Zealand'),
    ('Nicaragua'),
    ('Niger'),
    ('Nigeria'),
    ('North Korea'),
    ('Norway'),
    ('Oman'),
    ('Pakistan'),
    ('Palau'),
    ('Palestinian Territories'),
    ('Panama'),
    ('Papua New Guinea'),
    ('Paraguay'),
    ('Peru'),
    ('Philippines'),
    ('Poland'),
    ('Portugal'),
    ('Qatar'),
    ('Romania'),
    ('Russia'),
    ('Rwanda'),
    ('Saint Kitts and Nevis'),
    ('Saint Lucia'),
    ('St. Vincent/Grenadines'),
    ('Samoa'),
    ('San Marino'),
    ('Sao Tome and Principe'),
    ('SaudiA rabia'),
    ('Senegal'),
    ('Serbia'),
    ('Seychelles'),
    ('Sierra Leone'),
    ('Singapore'),
    ('Slovakia'),
    ('Slovenia'),
    ('Solomon Islands'),
    ('Somalia'),
    ('South Africa'),
    ('South Korea'),
    ('Spain'),
    ('Sri Lanka'),
    ('Sudan'),
    ('Suriname'),
    ('Swaziland'),
    ('Sweden'),
    ('Switzerland'),
    ('Syria'),
    ('Taiwan'),
    ('Tajikistan'),
    ('Tanzania'),
    ('Thailand'),
    ('Timor-Leste'),
    ('Togo'),
    ('Tonga'),
    ('Trinidad and Tobago'),
    ('Tunisia'),
    ('Turkey'),
    ('Turkmenistan'),
    ('Tuvalu'),
    ('Uganda'),
    ('Ukraine'),
    ('United Arab Emirates'),
    ('United Kingdom'),
    ('United States'),
    ('Uruguay'),
    ('Uzbekistan'),
    ('Vanuatu'),
    ('Venezuela'),
    ('Vietnam'),
    ('Yemen'),
    ('Zambia'),
    ('Zimbabwe');

insert into experiments (code, name, description) values (0, 'Impersonation', 'Temporarily change your profile to a character, famous person or historical figure so you can chat as if it was the same person typing it');