SELECT
 daten.id AS id,
 EXTRACT(year FROM daten.zeit_start) AS jahr,
 hochschule.beschr AS hochschule,
 traeger.bez AS traeger,
 bundesland.beschr AS bundesland,
 hochschulart.bez AS hochschulart,
 variable.bez AS variable,
 daten.wert AS wert,
 wert_einheit.beschr AS wert_einheit
FROM tag_link
LEFT JOIN daten
 ON tag_link.reihe_id = daten.id AND
    tag_link.tabelle_id = (SELECT id FROM tabelle WHERE bez = 'daten') AND
    tag_link.tag_id = (SELECT id FROM tag WHERE bez_lang = 'drittmittel_datensatz')
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'hochschule')
) hochschule ON daten.id = hochschule.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'traeger')
) traeger ON daten.id = traeger.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'hochschulart')
) hochschulart ON daten.id = hochschulart.daten_id
LEFT JOIN (
 SELECT *
 FROM daten_reichweite
 LEFT JOIN reichweite ON daten_reichweite.reichweite_id = reichweite.id
 WHERE reichweite.reichweite_typ_id = (SELECT id FROM reichweite_typ WHERE bez_lang = 'bundesland')
) bundesland ON daten.id = bundesland.daten_id
LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
LEFT JOIN variable ON daten.variable_id = variable.id
LEFT JOIN (
 SELECT daten_id, STRING_AGG(beschr, ';') AS beschr
 FROM home.daten_quelle daten_quelle
 LEFT JOIN home.quelle quelle ON daten_quelle.quelle_id = quelle.id
 GROUP BY daten_id
) quelle_agg ON daten.id = quelle_agg.daten_id
WHERE daten.id IS NOT NULL
