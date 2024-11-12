-- Adminer 4.8.1 MySQL 10.5.22-MariaDB dump

SET NAMES utf8;
SET time_zone = '+00:00';
SET foreign_key_checks = 0;
SET sql_mode = 'NO_AUTO_VALUE_ON_ZERO';

SET NAMES utf8mb4;

DROP TABLE IF EXISTS `catalogue`;
CREATE TABLE `catalogue` (
  `account` varchar(50) NOT NULL,
  `lang_messeges` varchar(30) NOT NULL,
  `count` int(11) NOT NULL,
  `name` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_czech_ci DEFAULT NULL,
  `members` int(11) DEFAULT NULL,
  `about` text DEFAULT NULL,
  `lang` varchar(30) DEFAULT NULL,
  `profile` text CHARACTER SET utf8mb4 COLLATE utf8mb4_czech_ci DEFAULT NULL,
  `subscribers` varchar(10) DEFAULT NULL,
  `photos` varchar(10) DEFAULT NULL,
  `videos` varchar(10) DEFAULT NULL,
  `files` varchar(10) DEFAULT NULL,
  `links` varchar(10) DEFAULT NULL,
  `downloaded` bit(1) NOT NULL,
  `tosearch` text DEFAULT NULL,
  `last_msg` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;


DROP TABLE IF EXISTS `channels`;
CREATE TABLE `channels` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(50) NOT NULL,
  `created` datetime DEFAULT NULL,
  `created_at` datetime DEFAULT current_timestamp(),
  `lang` varchar(30) DEFAULT NULL,
  `note` text DEFAULT NULL,
  `filesize` bigint(20) unsigned DEFAULT NULL,
  `file_min_msg` bigint(20) unsigned DEFAULT NULL,
  `file_max_msg` bigint(20) unsigned DEFAULT NULL,
  `last_known_message` bigint(20) unsigned DEFAULT NULL,
  `source` varchar(10) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;


DROP VIEW IF EXISTS `channels_info`;
CREATE TABLE `channels_info` (`channel_id` int(10) unsigned, `last_message` int(10) unsigned, `lm_posted` datetime, `username` varchar(50), `source` varchar(10), `name` varchar(256), `account` varchar(50), `subscribers` varchar(10), `catalogue_count` int(11), `channel_created` datetime, `last_known_message` bigint(20) unsigned, `file_min_msg` bigint(20) unsigned, `file_max_msg` bigint(20) unsigned, `msg_count` bigint(21), `forwarded_from` bigint(21), `forwarded_to` bigint(21), `lang` varchar(30));


DROP VIEW IF EXISTS `forward_info`;
CREATE TABLE `forward_info` ();


DROP TABLE IF EXISTS `messages`;
CREATE TABLE `messages` (
  `channel_id` int(10) unsigned NOT NULL,
  `msg_id` int(10) unsigned NOT NULL,
  `src_channel_id` int(10) unsigned DEFAULT NULL,
  `src_msg_id` int(10) unsigned DEFAULT NULL,
  `orig_channel_id` int(10) unsigned DEFAULT NULL,
  `orig_msg_id` int(10) unsigned DEFAULT NULL,
  `chain_length` int(10) unsigned NOT NULL DEFAULT 0,
  `posted` datetime DEFAULT NULL,
  `created_at` datetime DEFAULT current_timestamp(),
  PRIMARY KEY (`channel_id`,`msg_id`),
  KEY `src_channel_id` (`src_channel_id`,`src_msg_id`),
  KEY `src_channel_id_2` (`src_channel_id`,`src_msg_id`,`created_at`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;


DROP VIEW IF EXISTS `messages_info`;
CREATE TABLE `messages_info` (`channel_id` int(10) unsigned, `msg_id` int(10) unsigned, `posted` datetime, `src_channel_id` int(10) unsigned, `src_msg_id` int(10) unsigned, `src_posted` datetime);


DROP VIEW IF EXISTS `messages_with_lang`;
CREATE TABLE `messages_with_lang` (`channel_id` int(10) unsigned, `msg_id` int(10) unsigned, `src_channel_id` int(10) unsigned, `src_msg_id` int(10) unsigned, `orig_channel_id` int(10) unsigned, `orig_msg_id` int(10) unsigned, `chain_length` int(10) unsigned, `posted` datetime, `created_at` datetime, `channel_lang` varchar(30), `src_channel_lang` varchar(30));


DROP VIEW IF EXISTS `messages_with_preview`;
CREATE TABLE `messages_with_preview` (`id` int(10) unsigned, `channel_id` int(10) unsigned, `msg_id` int(10) unsigned, `src_channel_id` int(10) unsigned, `src_msg_id` int(10) unsigned, `orig_channel_id` int(10) unsigned, `orig_msg_id` int(10) unsigned, `chain_length` int(10) unsigned, `posted` datetime, `created_at` datetime, `URL` varchar(74));


DROP TABLE IF EXISTS `message_reactions`;
CREATE TABLE `message_reactions` (
  `channel_id` int(10) unsigned NOT NULL,
  `message_id` int(10) unsigned NOT NULL,
  `reaction_emo` varchar(5) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
  `count` int(10) unsigned NOT NULL,
  KEY `channel_id` (`channel_id`),
  KEY `reaction_emo` (`reaction_emo`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;


DROP TABLE IF EXISTS `channels_info`;
CREATE ALGORITHM=UNDEFINED SQL SECURITY DEFINER VIEW `channels_info` AS select `Y`.`id` AS `channel_id`,`M`.`msg_id` AS `last_message`,`M`.`posted` AS `lm_posted`,`CH`.`username` AS `username`,`CH`.`source` AS `source`,`C`.`name` AS `name`,`C`.`account` AS `account`,`C`.`subscribers` AS `subscribers`,`C`.`count` AS `catalogue_count`,`CH`.`created` AS `channel_created`,`CH`.`last_known_message` AS `last_known_message`,`CH`.`file_min_msg` AS `file_min_msg`,`CH`.`file_max_msg` AS `file_max_msg`,(select count(0) from `messages` `MS` where `Y`.`id` = `MS`.`channel_id`) AS `msg_count`,(select count(0) from `messages` `MS` where `Y`.`id` = `MS`.`src_channel_id`) AS `forwarded_from`,(select count(0) from `messages` `MS` where `Y`.`id` = `MS`.`channel_id` and `MS`.`src_channel_id` is not null) AS `forwarded_to`,`C`.`lang` AS `lang` from ((((select distinct `X`.`id` AS `id` from (select `channels`.`id` AS `id` from `channels` union select distinct `messages`.`channel_id` AS `channel_id` from `messages` union select distinct `messages`.`src_channel_id` AS `src_channel_id` from `messages` where `messages`.`src_channel_id` is not null) `X`) `Y` left join (select `M`.`channel_id` AS `channel_id`,`M`.`msg_id` AS `msg_id`,`M`.`posted` AS `posted` from (`messages` `M` join (select `messages`.`channel_id` AS `channel_id`,max(`messages`.`msg_id`) AS `max_msg_id` from `messages` group by `messages`.`channel_id`) `m2` on(`M`.`channel_id` = `m2`.`channel_id` and `M`.`msg_id` = `m2`.`max_msg_id`))) `M` on(`M`.`channel_id` = `Y`.`id`)) left join `channels` `CH` on(`CH`.`id` = `Y`.`id`)) left join `catalogue` `C` on(`C`.`account` = `CH`.`username`));

DROP TABLE IF EXISTS `forward_info`;
CREATE ALGORITHM=UNDEFINED SQL SECURITY DEFINER VIEW `forward_info` AS select `E`.`id` AS `channel_id`,`E`.`username` AS `username`,(select count(0) from `messages` `M` where `E`.`id` = `M`.`channel_id`) AS `msg_count`,(select count(0) from `messages` `M` where `E`.`id` = `M`.`src_channel_id`) AS `forwarded_count`,(select count(0) from `messages` `M` where `E`.`id` = `M`.`channel_id` and `M`.`src_channel_id` is not null) AS `forwards_accepted` from `emergent_channels` `E`;

DROP TABLE IF EXISTS `messages_info`;
CREATE ALGORITHM=UNDEFINED SQL SECURITY DEFINER VIEW `messages_info` AS select `M`.`channel_id` AS `channel_id`,`M`.`msg_id` AS `msg_id`,`M`.`posted` AS `posted`,`SRC`.`channel_id` AS `src_channel_id`,`SRC`.`msg_id` AS `src_msg_id`,`SRC`.`posted` AS `src_posted` from (`messages` `M` left join `messages` `SRC` on(`M`.`src_msg_id` = `SRC`.`msg_id` and `M`.`src_channel_id` = `SRC`.`channel_id`)) where `M`.`src_channel_id` is not null and `M`.`src_channel_id` is not null and `SRC`.`channel_id` is not null and `SRC`.`msg_id` is not null;

DROP TABLE IF EXISTS `messages_with_lang`;
CREATE ALGORITHM=UNDEFINED SQL SECURITY DEFINER VIEW `messages_with_lang` AS select `M`.`channel_id` AS `channel_id`,`M`.`msg_id` AS `msg_id`,`M`.`src_channel_id` AS `src_channel_id`,`M`.`src_msg_id` AS `src_msg_id`,`M`.`orig_channel_id` AS `orig_channel_id`,`M`.`orig_msg_id` AS `orig_msg_id`,`M`.`chain_length` AS `chain_length`,`M`.`posted` AS `posted`,`M`.`created_at` AS `created_at`,`CH1`.`lang` AS `channel_lang`,`CH2`.`lang` AS `src_channel_lang` from ((`messages` `M` left join `channels_info` `CH1` on(`M`.`channel_id` = `CH1`.`channel_id`)) left join `channels_info` `CH2` on(`M`.`src_channel_id` = `CH2`.`channel_id`)) where `M`.`src_channel_id` is not null;

DROP TABLE IF EXISTS `messages_with_preview`;
CREATE ALGORITHM=UNDEFINED SQL SECURITY DEFINER VIEW `messages_with_preview` AS select `C`.`id` AS `id`,`M`.`channel_id` AS `channel_id`,`M`.`msg_id` AS `msg_id`,`M`.`src_channel_id` AS `src_channel_id`,`M`.`src_msg_id` AS `src_msg_id`,`M`.`orig_channel_id` AS `orig_channel_id`,`M`.`orig_msg_id` AS `orig_msg_id`,`M`.`chain_length` AS `chain_length`,`M`.`posted` AS `posted`,`M`.`created_at` AS `created_at`,concat('https://t.me/',`C`.`username`,'/',`M`.`msg_id`) AS `URL` from (`messages` `M` left join `channels` `C` on(`M`.`channel_id` = `C`.`id`)) where `C`.`id` is not null;

-- 2024-11-12 21:04:50
