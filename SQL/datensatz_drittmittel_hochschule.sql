WITH
  datensatz AS (
   SELECT reihe_id as daten_id
   FROM tag_link
   WHERE
      tag_link.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'daten') AND
      tag_link.tag_id = (SELECT id FROM tag WHERE bez_lang = 'drittmittel_datensatz')
  ),
  hochschule AS (
   SELECT daten_id, beschr
   FROM daten_reichweite
   LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
   WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'hochschule')
  ),
  datensatz_quelle AS (
   SELECT daten_id, string_agg(beschr, ';') AS beschr
   FROM daten_quelle
   LEFT JOIN quelle quelle ON daten_quelle.quelle_id = quelle.id
   GROUP BY daten_id
  )

SELECT
 daten.id AS id,
 variable.beschr AS Variable,
 extract('year' FROM zeit_ende) AS Jahr,
 hochschule.beschr AS Hochschule,
 daten.wert AS Wert,
 wert_einheit.beschr AS Einheit,
 datensatz_quelle.beschr as Quelle
FROM datensatz
LEFT JOIN daten ON datensatz.daten_id = daten.id
LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
LEFT JOIN variable ON daten.variable_id = variable.id
LEFT JOIN hochschule ON daten.id = hochschule.daten_id
LEFT JOIN datensatz_quelle ON daten.id = datensatz_quelle.daten_id
WHERE daten.id IS NOT NULL AND hochschule.beschr IS NOT NULL
