include ViewCommon;
module DatenschutzErklaerung = {
  let title = {js|datenschutzerklärung|js};
  let section1 = {js|Diese Datenschutzerklärung klärt Sie über die Art, den Umfang und Zweck der Verarbeitung von personenbezogenen Daten (nachfolgend kurz „Daten“) innerhalb unseres Onlineangebotes und der mit ihm verbundenen Webseiten, Funktionen und Inhalte sowie externen Onlinepräsenzen, wie z.B. unser Social Media Profile auf (nachfolgend gemeinsam bezeichnet als „Onlineangebot“). Im Hinblick auf die verwendeten Begrifflichkeiten, wie z.B. „Verarbeitung“ oder „Verantwortlicher“ verweisen wir auf die Definitionen im Art. 4 der Datenschutzgrundverordnung (DSGVO).|js};
  let section2Heading = "Verantwortlicher";
  let section3Heading = "Arten der verarbeiteten Daten:";
  let section3 =
    <ul>
      <li> ("Bestandsdaten (z.B., Namen, Adressen)." |> text) </li>
      <li> ("Kontaktdaten (z.B., E-Mail, Telefonnummern)." |> text) </li>
      <li>
        (
          {js|Inhaltsdaten (z.B., Texteingaben, Fotografien, Videos).|js}
          |> text
        )
      </li>
      <li>
        (
          {js|Nutzungsdaten (z.B., besuchte Webseiten, Interesse an Inhalten, Zugriffszeiten).|js}
          |> text
        )
      </li>
      <li>
        (
          {js|Meta-/Kommunikationsdaten (z.B., Geräte-Informationen, IP-Adressen).|js}
          |> text
        )
      </li>
    </ul>;
  let section4Heading = {js|Kategorien betroffener Personen|js};
  let section4 = {js|Besucher und Nutzer des Onlineangebotes (Nachfolgend bezeichnen wir die betroffenen Personen zusammenfassend auch als „Nutzer“).|js};
};
