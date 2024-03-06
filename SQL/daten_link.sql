WITH
link_daten AS (
  SELECT
  id AS daten_id,
  zeit_einheit_id AS reihe_id,
  (SELECT id FROM tabelle WHERE bez = 'zeit_einheit') AS tabelle_id
  FROM daten
  UNION
  SELECT
  id AS daten_id,
  wert_einheit_id AS reihe_id,
  (SELECT id FROM tabelle WHERE bez = 'wert_einheit') AS tabelle_id
  FROM daten
  UNION
  SELECT
  id AS daten_id,
  variable_id AS reihe_id,
  (SELECT id FROM tabelle WHERE bez = 'variable') AS tabelle_id
  FROM daten
),
neu_reichweite_basis AS (
  SELECT
  reichweite.id AS reichweite_id,
  reichweite.reichweite_typ_id AS reichweite_typ_id,
  reichweite_typ.reichweite_klasse_id AS reichweite_klasse_id
  FROM reichweite
  LEFT JOIN reichweite_typ ON reichweite.reichweite_typ_id = reichweite_typ.id
),
neu_daten_reichweite AS (
  SELECT
  daten_reichweite.daten_id,
  neu_reichweite_basis.reichweite_id AS reihe_id,
  (SELECT id FROM tabelle WHERE bez = 'reichweite') AS tabelle_id
  FROM daten_reichweite
  LEFT JOIN neu_reichweite_basis ON daten_reichweite.reichweite_id = neu_reichweite_basis.reichweite_id
  UNION
  SELECT
  daten_reichweite.daten_id,
  neu_reichweite_basis.reichweite_typ_id AS reihe_id,
  (SELECT id FROM tabelle WHERE bez = 'reichweite_typ') AS tabelle_id
  FROM daten_reichweite
  LEFT JOIN neu_reichweite_basis ON daten_reichweite.reichweite_id = neu_reichweite_basis.reichweite_id
  UNION
  SELECT
  daten_reichweite.daten_id,
  neu_reichweite_basis.reichweite_klasse_id AS reihe_id,
  (SELECT id FROM tabelle WHERE bez = 'reichweite_klasse') AS tabelle_id
  FROM daten_reichweite
  LEFT JOIN neu_reichweite_basis ON daten_reichweite.reichweite_id = neu_reichweite_basis.reichweite_id
),
link_reichweite AS (
  SELECT *, 0 AS indirekt FROM neu_daten_reichweite
),
quelle_baum AS (
  WITH RECURSIVE cte AS (
    SELECT id, eltern_id, ARRAY[id] AS path, 0 AS generation_number
    FROM quelle
    WHERE eltern_id IS NULL
    UNION ALL
    SELECT q.id, q.eltern_id, cte.path || q.id, generation_number + 1 AS generation_number
    FROM quelle q
    JOIN cte ON cte.id = q.eltern_id
  ), expanded_paths AS (SELECT id, unnest(path) AS linked_to, generation_number FROM cte)
  SELECT DISTINCT e.id, e.linked_to, generation_number
  FROM expanded_paths e
),
neu_daten_quelle AS (
  SELECT
  daten_id,
  linked_to AS reihe_id,
  generation_number AS ebene,
  (select id FROM tabelle where bez = 'quelle') AS tabelle_id
  FROM daten_quelle
  left join quelle_baum on daten_quelle.quelle_id = quelle_baum.id
),
daten_link_teil1 AS (
  SELECT daten_id, reihe_id, tabelle_id, 0 AS ebene FROM link_daten
  UNION
  SELECT daten_id, reihe_id, tabelle_id, 0 AS ebene FROM link_reichweite
  UNION
  SELECT daten_id, reihe_id, tabelle_id, ebene AS ebene FROM neu_daten_quelle
),
tag_baum_basis AS (
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
),
tag_baum_prepared AS (
  SELECT id AS tag_id, linked_to, generation_number AS ebene FROM tag_baum_basis
),
tag_link_prepared AS (
  SELECT linked_to AS tag_id, tabelle_id, reihe_id, ebene
  FROM tag_link
  CROSS JOIN tag_baum_prepared
  where tag_baum_prepared.tag_id = tag_link.tag_id
),
daten_link_teil2 AS (
  SELECT daten_id, tag_id AS reihe_id, (select id FROM tabelle where bez = 'tag') AS tabelle_id, tlp.ebene, tlp.tabelle_id AS herkunft_tabelle_id
  FROM daten_link_teil1 d1
  CROSS JOIN tag_link_prepared tlp
  where d1.tabelle_id = tlp.tabelle_id and d1.reihe_id = tlp.reihe_id
),
daten_tag_basis AS (
  SELECT reihe_id AS daten_id, tag_id
  FROM tag_link
  where tag_link.tabelle_id = (select id FROM tabelle where bez = 'daten')
),
daten_tag AS (
  SELECT daten_id, linked_to AS reihe_id, (select id FROM tabelle where bez = 'tag') AS tabelle_id, ebene
  FROM daten_tag_basis
  CROSS JOIN tag_baum_prepared
  where daten_tag_basis.tag_id = tag_baum_prepared.tag_id
),
daten_link_final AS (
  SELECT daten_id, reihe_id, tabelle_id, ebene, null AS herkunft_tabelle_id FROM daten_link_teil1
  UNION
  SELECT daten_id, reihe_id, tabelle_id, ebene, herkunft_tabelle_id FROM daten_link_teil2
  UNION
  SELECT daten_id, reihe_id, tabelle_id, ebene, null AS herkunft_tabelle_id FROM daten_tag
)
SELECT * FROM daten_link_final


