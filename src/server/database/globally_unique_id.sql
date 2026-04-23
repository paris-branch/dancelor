-- @get
SELECT `type` FROM `globally_unique_id`
WHERE `id` = @id;

-- @register
INSERT INTO `globally_unique_id` (`id`, `type`)
VALUES (@id, @type_);
