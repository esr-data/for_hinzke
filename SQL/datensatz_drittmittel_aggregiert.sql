WITH
 datenbasis AS (
  SELECT *
  FROM daten
  WHERE daten.id IN (
   SELECT reihe_id as id
   FROM tag_link
   WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'daten') AND
         tag_id = (SELECT id FROM tag WHERE bez_lang = 'drittmittel_datensatz')
  )
 )
SELECT
 datenbasis.id AS daten_id,
 variable.beschr AS variable,
 date_part('year', datenbasis.zeit_ende) AS jahr,
 COALESCE(land.beschr, bundesland.beschr) AS bundesland,
 traeger.beschr AS traeger,
 hochschulart.beschr AS hochschulart,
 CAST(datenbasis.wert AS numeric) AS wert,
 wert_einheit.beschr AS wert_einheit
FROM datenbasis
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'hochschule')
) hochschule ON datenbasis.id = hochschule.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'traeger')
) traeger ON datenbasis.id = traeger.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'hochschulart')
) hochschulart ON datenbasis.id = hochschulart.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'bundesland')
) bundesland ON datenbasis.id = bundesland.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'land')
) land ON datenbasis.id = land.daten_id
LEFT JOIN wert_einheit ON datenbasis.wert_einheit_id = wert_einheit.id
LEFT JOIN variable ON datenbasis.variable_id = variable.id
WHERE datenbasis.id IS NOT NULL AND hochschule.beschr IS NULL
ORDER BY
 jahr DESC,
 land DESC,
 bundesland DESC NULLS LAST,
 traeger DESC NULLS LAST,
 hochschulart ASC NULLS LAST,
 variable DESC
