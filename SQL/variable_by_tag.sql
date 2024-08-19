WITH
vtl AS (
	SELECT *
	FROM view_tag_link
	%s
),
vtl_agg AS (
	SELECT reihe_id as variable_id, MIN(ebene) as ebene
	FROM tag_link
	LEFT JOIN vtl ON tag_link.tag_id = vtl.tag_id
	WHERE tag_link.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable') AND
	      vtl.tag_id IS NOT NULL
	GROUP BY variable_id
),
tl AS (
	SELECT tl.tag_id, tl.reihe_id as variable_id
	FROM tag_link tl
	WHERE tl.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable') AND
	      tl.tag_id IN (SELECT tag_id FROM vtl)
),
variable_stats AS (
	SELECT variable_id, COUNT(variable_id) as n, MIN(extract(year from zeit_start)) as zeit_min, MAX(extract(year from zeit_ende)) as zeit_max
	FROM daten
	GROUP BY variable_id
)
SELECT variable.beschr, vs.*, vtl_agg.ebene
FROM variable
LEFT JOIN variable_stats vs ON variable.id = vs.variable_id
LEFT JOIN vtl_agg ON variable.id = vtl_agg.variable_id
WHERE
 vs.n IS NOT NULL AND
 variable.id IN (SELECT variable_id FROM tl)
ORDER BY ebene ASC, n DESC
