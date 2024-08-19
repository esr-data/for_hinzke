WITH tag_baum_basis AS (
  WITH RECURSIVE cte AS (
    SELECT id, eltern_id, ARRAY[id] AS path, 0 AS generation_number
    FROM tag
    WHERE eltern_id IS NULL
    UNION ALL
    SELECT q.id, q.eltern_id, cte.path || q.id, generation_number + 1 AS generation_number
    FROM tag q
    JOIN cte ON cte.id = q.eltern_id
  ), expanded_paths AS (SELECT id, unnest(path) AS linked_to, generation_number FROM cte)
  SELECT DISTINCT e.id, e.linked_to, generation_number
  FROM expanded_paths e
)
SELECT id AS tag_id, linked_to as tag_link_id, generation_number AS ebene FROM tag_baum_basis
