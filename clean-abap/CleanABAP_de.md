# Clean ABAP

> [**Deutsch**](CleanABAP_de.md)
> &nbsp;·&nbsp;
> [Englisch](CleanABAP.md)

Dieser Leitfaden ist eine Adaption des Standardwerks [Robert C. Martins _Clean Code_]
an [ABAP](https://de.wikipedia.org/wiki/ABAP).

Das [Cheat Sheet](cheat-sheet/CheatSheet.md) ist eine druckoptimierte Version.

[Robert C. Martins _Clean Code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

> Diese Übersetzung spiegelt den Stand
> vom [14. November 2019 des englischen Originals](https://github.com/SAP/styleguides/tree/72ecf7fd7d41151d5bbca29020d4ec9de953db8c) wieder.
> Den neuesten Stand finden Sie in der
> [aktuellen Fassung des englischen Originals](CleanABAP.md).

## Inhalt

- [How-to](#how-to)
   - [How-to: Erste Schritte mit Clean Code](#how-to-erste-schritte-mit-clean-code)
   - [How-to: Refactoring von Legacy Code](#how-to-refactoring-von-legacy-code)
   - [How-to: Automatische Prüfung](#how-to-automatische-prfung)
   - [How-to: Weitere Leitfäden](#how-to-weitere-leitfden)
   - [How-to: Kritik, Änderungsvorschläge](#how-to-kritik-nderungsvorschlge)
- [Namen](#namen)
   - [Aussagekräftige Namen verwenden](#aussagekrftige-namen-verwenden)
   - [Begriffe der Lösungsdomäne und Problemdomäne bevorzugen](#begriffe-der-lsungsdomne-und-problemdomne-bevorzugen)
   - [Plural verwenden](#plural-verwenden)
   - [Aussprechbare Namen verwenden](#aussprechbare-namen-verwenden)
   - [Abkürzungen vermeiden](#abkrzungen-vermeiden)
   - [Abkürzungen konsistent verwenden](#abkrzungen-konsistent-verwenden)
   - [Nomen für Klassen, Verben für Methoden](#nomen-fr-klassen-verben-fr-methoden)
   - [Stoppwörter wie „Daten“, „Info“, „Objekt“ vermeiden](#stoppwrter-wie-daten-info-objekt-vermeiden)
   - [Ein Wort pro Konzept wählen](#ein-wort-pro-konzept-whlen)
   - [Musternamen nur mit Absicht verwenden](#musternamen-nur-mit-absicht-verwenden)
   - [Codierungen vermeiden, insbes. Ungarische Notation und Präfixe](#codierungen-vermeiden-insbes-ungarische-notation-und-prfixe)
- [Sprache](#language)
   - [Vorsicht bei älteren ABAP-Releases](#mind-the-legacy)
   - [Performance beachten](#mind-the-performance)
   - [Besser Objektorientierung als prozedurale Programmierung](#prefer-object-orientation-to-procedural-programming)
   - [Besser funktionale als prozedurale Sprachkonstruktion](#prefer-functional-to-procedural-language-constructs)
   - [Obsolete Sprachelemente vermeiden](#avoid-obsolete-language-elements)
   - [Entwurfsmuster mit Bedacht einsetzen](#use-design-patterns-wisely)
- [Konstanten](#constants)
   - [Konstanten statt magischer Zahlen verwenden](#use-constants-instead-of-magic-numbers)
   - [Besser Enumerationsklassen als Konstanten-Interfaces](#prefer-enumeration-classes-to-constants-interfaces)
   - [Konstanten gruppieren, wenn Sie auf Enumerationsklassen verzichten](#if-you-dont-use-enumeration-classes-group-your-constants)
- [Variablen](#variables)
   - [Besser Inline-Deklaration als voranstehende Deklaration](#prefer-inline-to-up-front-declarations)
   - [Keine Inline-Deklaration in optionalen Verzweigungen](#dont-declare-inline-in-optional-branches)
   - [Keine Verkettung von voranstehenden Deklarationen](#do-not-chain-up-front-declarations)
   - [Besser REF TO als FIELD-SYMBOL](#prefer-ref-to-to-field-symbol)
- [Tabellen](#tables)
   - [Korrekte Tabellenart verwenden](#use-the-right-table-type)
   - [DEFAULT KEY vermeiden](#avoid-default-key)
   - [Besser INSERT INTO TABLE als APPEND TO](#prefer-insert-into-table-to-append-to)
   - [Besser LINE_EXISTS als READ TABLE oder LOOP AT](#prefer-line_exists-to-read-table-or-loop-at)
   - [Besser READ TABLE als LOOP AT](#prefer-read-table-to-loop-at)
   - [Besser LOOP AT WHERE als verschachteltes IF](#prefer-loop-at-where-to-nested-if)
   - [Überflüssige Lesezugriffe auf Tabelle vermeiden](#avoid-unnecessary-table-reads)
- [Strings](#strings)
   - [Literale mit ` definieren](#use--to-define-literals)
   - [Text mit | assemblieren](#use--to-assemble-text)
- [Boolesche Ausdrücke](#booleans)
   - [Boolesche Ausdrücke mit Bedacht einsetzen](#use-booleans-wisely)
   - [ABAP_BOOL für boolesche Ausdrücke verwenden](#use-abap_bool-for-booleans)
   - [ABAP_TRUE und ABAP_FALSE für Vergleiche verwenden](#use-abap_true-and-abap_false-for-comparisons)
   - [XSDBOOL für boolesche Variablen verwenden](#use-xsdbool-to-set-boolean-variables)
- [Bedingungen](#conditions)
   - [Bedingungen nach Möglichkeit positiv definieren](#try-to-make-conditions-positive)
   - [Besser IS NOT als NOT IS](#prefer-is-not-to-not-is)
   - [Komplexe Bedingungen zerlegen](#consider-decomposing-complex-conditions)
   - [Komplexe Bedingungen extrahieren](#consider-extracting-complex-conditions)
- [IF](#ifs)
   - [Keine leeren IF-Verzweigungen](#no-empty-if-branches)
   - [Bei mehreren Alternativbedingungen besser CASE als ELSE IF](#prefer-case-to-else-if-for-multiple-alternative-conditions)
   - [Schachtelungstiefe so gering wie möglich halten](#keep-the-nesting-depth-low)
- [Reguläre Ausdrücke](#regular-expressions)
   - [Besser einfachere Methoden als reguläre Ausdrücke](#prefer-simpler-methods-to-regular-expressions)
   - [Besser Basisprüfungen als reguläre Ausdrücke](#prefer-basis-checks-to-regular-expressions)
   - [Komplexe reguläre Ausdrücke assemblieren](#consider-assembling-complex-regular-expressions)
- [Klassen](#classes)
   - [Klassen: Objektorientierung](#classes-object-orientation)
      - [Besser Objekte als statische Klassen](#prefer-objects-to-static-classes)
      - [Besser Komposition als Vererbung](#prefer-composition-to-inheritance)
      - [Kein Mix von Stateful und Stateless in derselben Klasse](#dont-mix-stateful-and-stateless-in-the-same-class)
   - [Scope](#scope)
      - [Global ist Standard, lokal nur im Bedarfsfall](#global-by-default-local-only-where-appropriate)
      - [FINAL, wenn keine Vererbung vorgesehen](#final-if-not-designed-for-inheritance)
      - [Mitglieder standardmäßig PRIVATE, nur im Bedarfsfall PROTECTED](#members-private-by-default-protected-only-if-needed)
      - [Unveränderlichkeit anstelle des Getter erwägen](#consider-using-immutable-instead-of-getter)
      - [READ-ONLY sparsam verwenden](#use-read-only-sparingly)
   - [Konstruktoren](#constructors)
      - [Besser NEW als CREATE OBJECT](#prefer-new-to-create-object)
      - [Bei globaler Klasse CREATE PRIVATE lassen Sie den CONSTRUCTOR öffentlich](#if-your-global-class-is-create-private-leave-the-constructor-public)
      - [Besser mehrere statische Erstellungsmethoden als optionale Parameter](#prefer-multiple-static-creation-methods-to-optional-parameters)
      - [Aussagekräftige Namen bei mehreren Erstellungsmethoden verwenden](#use-descriptive-names-for-multiple-creation-methods)
      - [Singletons nur, wenn Multi-Instanzen keinen Sinn machen](#make-singletons-only-where-multiple-instances-dont-make-sense)
- [Methoden](#methods)
   - [Aufrufe](#calls)
      - [Besser funktionale als prozedurale Aufrufe](#prefer-functional-to-procedural-calls)
      - [RECEIVING weglassen](#omit-receiving)
      - [Optionales Schlüsselwort EXPORTING weglassen](#omit-the-optional-keyword-exporting)
      - [Parametername in einzelnen Parameteraufrufen weglassen](#omit-the-parameter-name-in-single-parameter-calls)
      - [Eigenbezug me beim Aufruf einer Instanzmethode weglassen](#omit-the-self-reference-me-when-calling-an-instance-method)
   - [Methoden: Objektorientierung](#methods-object-orientation)
      - [Besser Instanzmethode als statische Methode](#prefer-instance-to-static-methods)
      - [Öffentliche Instanzmethoden sollten Teil einer Schnittstelle sein](#public-instance-methods-should-be-part-of-an-interface)
   - [Parameteranzahl](#parameter-number)
      - [So wenig IMPORTING-Parameter wie möglich, im Bestfall weniger als drei](#aim-for-few-importing-parameters-at-best-less-than-three)
      - [Besser Methoden aufteilen als OPTIONAL-Parameter hinzufügen](#split-methods-instead-of-adding-optional-parameters)
      - [PREFERRED PARAMETER sparsam verwenden](#use-preferred-parameter-sparingly)
      - [RETURN, EXPORT oder CHANGE - nur eins davon](#return-export-or-change-exactly-one-parameter)
   - [Parametertypen](#parameter-types)
      - [Besser RETURNING als EXPORTING](#prefer-returning-to-exporting)
      - [RETURNING von großen Tabellen ist in der Regel problemlos](#returning-large-tables-is-usually-okay)
      - [RETURNING oder EXPORTING oder CHANGING verwenden, jedoch keine Kombination](#use-either-returning-or-exporting-or-changing-but-not-a-combination)
      - [CHANGING sparsam verwenden, wo geeignet](#use-changing-sparingly-where-suited)
      - [Aufgeteilte Methode statt boolescher Eingabeparameter](#split-method-instead-of-boolean-input-parameter)
   - [Parameternamen](#parameter-names)
      - [RETURNING-Parameter evtl. RESULT nennen](#consider-calling-the-returning-parameter-result)
   - [Parameterinitialisierung](#parameter-initialization)
      - [EXPORTING-Referenzparameter löschen oder überschreiben](#clear-or-overwrite-exporting-reference-parameters)
         - [Vorsicht bei identischer Ein- und Ausgabe](#take-care-if-input-and-output-could-be-the-same)
      - [VALUE-Parameter nicht löschen](#dont-clear-value-parameters)
   - [Methodenrumpf](#method-body)
      - [Mache eine Sache zur Zeit, und mache sie gut](#do-one-thing-do-it-well-do-it-only)
      - [Glücklicher Pfad oder Fehlerbehebung, nicht Beides](#focus-on-the-happy-path-or-error-handling-but-not-both)
      - [Eine Abstraktionsebene tiefer steigen](#descend-one-level-of-abstraction)
      - [Methoden klein halten](#keep-methods-small)
   - [Kontrollfluss](#control-flow)
      - [Früh scheitern](#fail-fast)
      - [CHECK vs. RETURN](#check-vs-return)
      - [CHECK an anderer Stelle vermeiden](#avoid-check-in-other-positions)
- [Fehlerbehebung](#error-handling)
   - [Meldungen](#messages)
      - [Meldungen leicht auffindbar machen](#make-messages-easy-to-find)
   - [Rückgabecodes](#return-codes)
      - [Ausnahmen statt Rückgabecodes](#prefer-exceptions-to-return-codes)
      - [Alle Fehler abfangen](#dont-let-failures-slip-through)
   - [Ausnahmen](#exceptions)
      - [Ausnahmen sind für Fehler gedacht, nicht für den Normalfall](#exceptions-are-for-errors-not-for-regular-cases)
      - [Klassenbasierte Ausnahmen verwenden](#use-class-based-exceptions)
   - [Ausnahme absetzen](#throwing)
      - [Eigene übergeordnete Klassen verwenden](#use-own-super-classes)
      - [Einen Ausnahmetyp zur Zeit absetzen](#throw-one-type-of-exception)
      - [Übersichtlichere Fehlersituationen mit untergeordneten Klassen](#use-sub-classes-to-enable-callers-to-distinguish-error-situations)
      - [CX_STATIC_CHECK für überschaubare Ausnahmen absetzen](#throw-cx_static_check-for-manageable-exceptions)
      - [CX_NO_CHECK für gewöhnlich nicht behebbare Situationen absetzen](#throw-cx_no_check-for-usually-unrecoverable-situations)
      - [CX_DYNAMIC_CHECK für vermeidbare Ausnahmen absetzen](#consider-cx_dynamic_check-for-avoidable-exceptions)
      - [Dump für schwerwiegende, nicht behebbare Situationen absetzen](#dump-for-totally-unrecoverable-situations)
      - [Besser RAISE EXCEPTION NEW als RAISE EXCEPTION TYPE](#prefer-raise-exception-new-to-raise-exception-type)
   - [Ausnahmen abfangen](#catching)
      - [Externe Ausnahmen umschließen, um das Eindringen in Ihren Code zu verhindern](#wrap-foreign-exceptions-instead-of-letting-them-invade-your-code)
- [Kommentare](#comments)
   - [In Code ausdrücken, nicht in Kommentaren](#express-yourself-in-code-not-in-comments)
   - [Kommentare sind keine Ausrede für schlechte Namenswahl](#comments-are-no-excuse-for-bad-names)
   - [Methoden statt Kommentaren zur Code-Segmentierung verwenden](#use-methods-instead-of-comments-to-segment-your-code)
   - [Mit Kommentaren das Warum, nicht das Was erläutern](#write-comments-to-explain-the-why-not-the-what)
   - [Design gehört in das Design-Dokument, nicht in den Code](#design-goes-into-the-design-documents-not-the-code)
   - [Kommentare mit ", nicht mit * markieren](#comment-with--not-with-)
   - [Kommentare gehören vor die Anweisung, auf die sie sich beziehen](#put-comments-before-the-statement-they-relate-to)
   - [Code löschen, nicht kommentieren](#delete-code-instead-of-commenting-it)
   - [FIXME, TODO und XXX verwenden, und Ihre ID hinzufügen](#use-fixme-todo-and-xxx-and-add-your-id)
   - [Kein Kommentar zu Methodensignatur und Ende](#dont-add-method-signature-and-end-of-comments)
   - [Meldungstexte nicht in Kommentaren wiederholen](#dont-duplicate-message-texts-as-comments)
   - [ABAP Doc nur für öffentliche APIs](#abap-doc-only-for-public-apis)
   - [Besser Pragmas als Pseudokommentare](#prefer-pragmas-to-pseudo-comments)
- [Formatierung](#formatting)
   - [Konsistent sein](#be-consistent)
   - [Zum Lesen optimieren, nicht zum Schreiben](#optimize-for-reading-not-for-writing)
   - [Pretty Printer vor der Aktivierung verwenden](#use-the-pretty-printer-before-activating)
   - [Ihre Pretty-Printer-Teameinstellungen verwenden](#use-your-pretty-printer-team-settings)
   - [Maximal eine Anweisung pro Zeile](#no-more-than-one-statement-per-line)
   - [Vernünftige Zeilenlänge einhalten](#stick-to-a-reasonable-line-length)
   - [Ihren Code kondensieren](#condense-your-code)
   - [Nur eine Leerzeile zum Trennen](#add-a-single-blank-line-to-separate-things-but-not-more)
   - [Keine exzessiven Leerzeilen](#dont-obsess-with-separating-blank-lines)
   - [Zuordnung zum selben Objekt verdeutlichen](#align-assignments-to-the-same-object-but-not-to-different-ones)
   - [Klammern am Zeilenende schließen](#close-brackets-at-line-end)
   - [Einzelne Parameteraufrufe auf einer Zeile belassen](#keep-single-parameter-calls-on-one-line)
   - [Parameter hinter dem Aufruf angeben](#keep-parameters-behind-the-call)
   - [Bei Zeilenumbruch Parameter unter dem Aufruf einrücken](#if-you-break-indent-parameters-under-the-call)
   - [Zeilenumbruch bei mehreren Parametern](#line-break-multiple-parameters)
   - [Parameter anordnen](#align-parameters)
   - [Aufruf auf eine neue Zeile umbrechen, wenn die Zeile zu lang wird](#break-the-call-to-a-new-line-if-the-line-gets-too-long)
   - [Einrücken und Tabulator verwenden](#indent-and-snap-to-tab)
   - [Inline-Deklarationen wie Methodenaufrufe einrücken](#indent-in-line-declarations-like-method-calls)
   - [Type-Klauseln nicht ausrichten](#dont-align-type-clauses)
- [Test](#testing)
   - [Grundlagen](#principles)
      - [Testbaren Code schreiben](#write-testable-code)
      - [Nachstellen erlaubt](#enable-others-to-mock-you)
      - [Regeln für die Lesbarkeit](#readability-rules)
      - [Keine Kopien oder Testreports](#dont-make-copies-or-write-test-reports)
      - [Nur Public-Parts testen](#test-publics-not-private-internals)
      - [Zum Thema Quelltextabdeckung](#dont-obsess-about-coverage)
   - [Testklassen](#test-classes)
      - [Lokale Testklassen nach ihrem Zweck benennen](#call-local-test-classes-by-their-purpose)
      - [Tests in lokale Klassen integrieren](#put-tests-in-local-classes)
      - [Hilfsmethoden in Hilfeklassen integrieren](#put-help-methods-in-help-classes)
      - [Testklassen ausführen](#how-to-execute-test-classes)
   - [Getesteter Code](#code-under-test)
      - [Sinnvolle Code-Namen oder Standardname CUT](#name-the-code-under-test-meaningfully-or-default-to-cut)
      - [Schnittstellen testen, nicht Klassen](#test-interfaces-not-classes)
      - [Aufruf an getesteten Code in seine eigene Methode extrahieren](#extract-the-call-to-the-code-under-test-to-its-own-method)
   - [Injection](#injection)
      - [Abhängigkeitsumkehr zum Einbringen von Testattrappen verwenden](#use-dependency-inversion-to-inject-test-doubles)
      - [ABAP-Testattrappe verwenden](#consider-to-use-the-tool-abap-test-double)
      - [Von Test-Tools unterstützen lassen](#exploit-the-test-tools)
      - [Testseams als temporäre Behelfslösung verwenden](#use-test-seams-as-temporary-workaround)
      - [Mit LOCAL FRIENDS auf Abhängigkeitsumkehr-Konstruktor zugreifen](#use-local-friends-to-access-the-dependency-inverting-constructor)
      - [LOCAL FRIENDS nicht zum Eindringen in den getesteten Code missbrauchen](#dont-misuse-local-friends-to-invade-the-tested-code)
      - [Produktiven Code nicht zugunsten Testbarkeit ändern](#dont-change-the-productive-code-to-make-the-code-testable)
      - [Keine Unterklassen zum Nachstellen von Methoden](#dont-sub-class-to-mock-methods)
      - [Nichts Unnötiges nachstellen](#dont-mock-stuff-thats-not-needed)
      - [Keine Test-Frameworks aufbauen](#dont-build-test-frameworks)
   - [Testmethoden](#test-methods)
      - [Testmethodennamen: was ist gegeben, was wird erwartet](#test-method-names-reflect-whats-given-and-expected)
      - [Given/When/Then verwenden](#use-given-when-then)
      - [„When“ ist genau ein Aufruf](#when-is-exactly-one-call)
      - [TEARDOWN nur, wenn es sein muss](#dont-add-a-teardown-unless-you-really-need-it)
   - [Testdaten](#test-data)
      - [Einfach erkennbare Bedeutung](#make-it-easy-to-spot-meaning)
      - [Einfach erkennbare Abweichungen](#make-it-easy-to-spot-differences)
      - [Konstanten zur Beschreibung von Zweck und Bedeutung der Testdaten verwenden](#use-constants-to-describe-purpose-and-importance-of-test-data)
   - [Assertionen](#assertions)
      - [Wenige, fokussierte Assertionen](#few-focused-assertions)
      - [Korrekten Assertionstyp verwenden](#use-the-right-assert-type)
      - [Inhalt, nicht Menge zusichern](#assert-content-not-quantity)
      - [Qualität, nicht Inhalt zusichern](#assert-quality-not-content)
      - [FAIL zum Prüfen erwarteter Ausnahmen verwenden](#use-fail-to-check-for-expected-exceptions)
      - [Unerwartete Ausnahmen nicht vergeblich abfangen, sondern weiterleiten](#forward-unexpected-exceptions-instead-of-catching-and-failing)
      - [Angepasste Assertionen: Code verkürzen, Doppeltes vermeiden](#write-custom-asserts-to-shorten-code-and-avoid-duplication)

## How-to

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#how-to)

### How-to: Erste Schritte mit Clean Code

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [How-to](#how-to) > [Dieser Abschnitt](#how-to-get-started-with-clean-code)

Wenn das Thema Clean Code neu für Sie ist, empfehlen wir, zunächst das Buch [Robert C. Martins _Clean Code_] zu lesen. Zusätzlich kann Ihnen die didaktisch aufbereitete Schritt-für-Schritt-Einführung der Initiative [Clean Code Developer ](https://clean-code-developer.com/) den Einstieg in das allgemeine Thema erleichtern.

Wir empfehlen Ihnen, mit einfach verständlichen und weithin akzeptierten Dingen zu beginnen, wie z.B. [booleschen Ausdrücken](#booleans), [Bedingungen](#conditions) und [IFs](#ifs).

Sie werden wahrscheinlich am meisten vom Abschnitt [Methoden](#methods) profitieren, insbesondere von den Themen [Mache eine Sache zur Zeit, und mache sie gut](#do-one-thing-do-it-well-do-it-only) und [Methoden klein halten](#keep-methods-small), weil diese zu einer enormen Verbesserung der Gesamtstruktur Ihres Codes beitragen.

Einige der hier behandelten Themen können zu kontroversen Diskussionen in Teams führen, die zwar erfahren, jedoch nicht mit Clean Code vertraut sind. Diese Themen sind völlig „unbedenklich“, manchen Beteiligten kann es jedoch anfänglich schwerfallen, sich mit ihnen anzufreunden.

Gehen Sie zu einem späteren Zeitpunkt zu diesen eher kontroversen Themen über.
Insbesondere die Themen [Kommentare](#comments), [Namen](#names) und [Formatierung](#formatting)
können zu nahezu fanatischen Diskussionen führen und sollten nur von Teams angegangen werden, die sich bereits von den positiven Auswirkungen des Clean Code überzeugt haben.

### How-to: Refactoring von Legacy Code

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [How-to](#how-to) > [Dieser Abschnitt](#how-to-refactor-legacy-code)

Die Themen [Boolesche Ausdrücke](#booleans), [Bedingungen](#conditions), [IFs](#ifs)
und [Methoden](#methods) zahlen sich am meisten aus, wenn Sie an einem Legacy-Projekt mit Massen von Code arbeiten, den Sie nicht ändern können oder wollen, weil die Empfehlungen in diesen Themen ohne Konflikte auf den neuen Code angewendet werden können.

Das Thema [Namen](#names) ist sehr anspruchsvoll für Legacy-Projekte, da es hier zu einem Bruch zwischen altem und neuem Code kommen kann. Dies kann so weit führen, dass die Informationen in Abschnitten wie [Codierungen vermeiden, insbes. Ungarische Notation und Präfixe](#avoid-encodings-esp-hungarian-notation-and-prefixes)
besser ignoriert werden.

Wir haben mit einem Vier-Schritte-Plan für das Refactoring gute Ergebnisse erzielt:

1. Holen Sie das Team an Bord. Kommunizieren und erläutern Sie den neuen Stil, und stellen Sie sicher, dass jedes Mitglied des Projektteams damit einverstanden ist.
Sie müssen nicht alle Richtlinien auf einmal festschreiben. Beginnen Sie einfach mit einem kleinen unstrittigen Teilbereich und entwickeln Sie sich von dort aus weiter.

2. Befolgen Sie in Ihrer täglichen Arbeitsroutine die _Pfadfinderregel_ „Verlasse den Campingplatz sauberer als du ihn vorgefunden hast“, in Bezug auf Clean Code _Hinterlassen Sie den Code immer besser, als Sie ihn vorgefunden haben_.
Übertreiben Sie es nicht, indem Sie stundenlang „den gesamten Campingplatz aufräumen“.
Wenden Sie einfach ein paar Minuten zusätzlich auf und beobachten Sie,
wie sich die Verbesserungen im Zeitverlauf akkumulieren.

3. Bauen Sie von Zeit zu Zeit _saubere Inseln_ auf: Wählen Sie ein kleines Objekt oder eine kleine Komponente aus und versuchen Sie, dieses Objekt oder die Komponente in allen Aspekten „sauber“ zu machen. Diese Inseln demonstrieren den Nutzen von dem, was Sie tun, und bilden eine zuverlässig getestete Basis für das weitere Refactoring.

4. Sprechen Sie drüber. Ganz gleich, ob Sie [Fagan-Code-Inspektionen](https://en.wikipedia.org/wiki/Fagan_inspection) alter Schule aufsetzen, oder Info-Sessions bzw. Forumsdiskussionen in Ihrem bevorzugten Chat Tool veranstalten:
Sie müssen über Ihre Erfahrungen und das Gelernte sprechen, damit in Ihrem Team ein gemeinsames Verständnis wachsen kann.

### How-to: Automatische Prüfung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [How-to](#how-to) > [Dieser Abschnitt](#how-to-check-automatically)

Es gibt kein umfassendes Paket mit statischen Code-Prüfungen, von denen die hier beschriebenen Anti-Pattern automatisch entdeckt werden könnten.

ABAP Test Cockpit, Code Inspector, Extended Check und CheckMan stellen einige Prüfungen bereit, die Ihnen beim Aufspüren bestimmter Probleme helfen können.

[abapOpenChecks](https://github.com/larshp/abapOpenChecks), eine Open-Source-Sammlung von Code-Inspector-Prüfungen, deckt ebenfalls einige der hier beschriebenen Anti-Pattern ab.

[abaplint](https://github.com/abaplint/abaplint) ist eine Open-Source-Wiederaufrollung des ABAP-Parsers. Das Tool funktioniert ohne SAP-System und ist für Code vorgesehen, der mit abapGit serialisiert wurde. Das Tool bietet Mehrfachintegration (GitHub-Aktionen, Jenkins, Text-Editoren...), deckt einige der Anti-Pattern ab und kann außerdem zum Prüfen der Formatierung und Code-Konventionen verwendet werden.

### How-to: Weitere Leitfäden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [How-to](#how-to) > [Dieser Abschnitt](#how-to-relate-to-other-guides)

Unser Leitfaden folgt dem _Geist_ des Clean Code. Das bedeutet, wir haben einige Anpassungen an die Programmiersprache ABAP  vorgenommen, wie z.B. [CX_STATIC_CHECK für überschaubare Ausnahmen absetzen](#throw-cx_static_check-for-manageable-exceptions).

Einige Fakten stammen aus den [ABAP-Programmierrichtlinien](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm), mit denen dieser Leitfaden größtenteils kompatibel ist. Abweichungen werden explizit hervorgehoben und sind immer im Geist des Clean Code verankert.

Dieser Leitfaden respektiert außerdem die [DSAG-Empfehlungen für die ABAP-Entwicklung](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf), auch wenn der vorliegende Leitfaden in den meisten Einzelheiten sehr viel präziser ist.

### How-to: Kritik, Änderungsvorschläge

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [How-to](#how-to) > [Dieser Abschnitt](#how-to-disagree)

Dieser Leitfaden richtet sich an Leser, die bereits mit Clean Code vertraut sind oder sich gerade damit vertraut machen, mit einem starken Fokus auf der Anwendung von Clean Code _speziell auf ABAP_.

Bitte beachten Sie daher, dass wir nicht alle Konzepte in derselben Breite und Tiefe wie im Originalwerk und den themenbezogenen Ressourcen vorgestellt haben. Diese Quellen sind weiterhin lohnenswert, insbesondere, wenn Sie mit dem hier Beschriebenen nur deshalb nicht einverstanden sind, weil es nicht ausreichend erläutert wurde. Verwenden Sie die Links in den Abschnitten, um Hintergrundinformationen zu unseren Empfehlungen zu lesen.

Sie sind frei, alles hier Beschriebene zu hinterfragen und abzulehnen. Eine der Säulen des Clean Code ist, dass _das Team regiert_. Geben Sie jedoch den Dingen eine faire Chance, bevor Sie sie ablehnen.

[CONTRIBUTING.md](../CONTRIBUTING.md) enthält Vorschläge, wie Sie diesen Leitfaden ändern bzw. in kleineren Details von ihm abweichen können.

## Namen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#names)

### Aussagekräftige Namen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#use-descriptive-names)

Verwenden Sie Namen, die Inhalt und Bedeutung vermitteln.

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

Konzentrieren Sie sich nicht auf den Datentyp oder das technische Coding. Sie tragen wenig zum Verständnis des Codes bei.

```ABAP
" anti-pattern
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[Versuchen Sie nicht, eine ungeeignete Namenswahl durch Kommentare wieder gutzumachen.](#comments-are-no-excuse-for-bad-names)

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Zweckbeschreibende Namen wählen_ in [Robert C. Martins _Clean Code_].

### Begriffe der Lösungsdomäne und Problemdomäne bevorzugen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#prefer-solution-domain-and-problem-domain-terms)

Suchen Sie nach geeigneten Namen in der Lösungsdomäne, z.B. Begriffe der Informatik wie „Queue“ oder „Tree“, und in der Problemdomäne, z.B. betriebswirtschaftliche Begriffe wie „Konto“ oder „Ledger“.

Für Layer, die betriebswirtschaftliche Funktionen bereitstellen, eignet sich am besten eine Benennung nach der Problemdomäne. Dies gilt insbesondere für Komponenten, die mit Domain-Driven Design entworfen wurden, wie z.B. APIs und Business Objects.

Layer, die hauptsächlich technische Funktionen bereitstellen, wie z.B. Factory-Klassen und abstrakte Algorithmen, klingen am besten, wenn sie nach der Lösungsdomäne benannt werden.

Versuchen Sie auf keinen Fall, Ihre eigene Sprache zu erfinden. Informationen müssen zwischen Entwicklern, Product Owners, Partnern und Kunden ausgetauscht werden. Wählen Sie daher Namen, die sich für alle Beteiligten ohne Spezialwörterbuch erschließen.

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Namen der Lösungsdomäne verwenden_ und _[...]:
> Namen der Problemdomäne verwenden_ in [Robert C. Martins _Clean Code_].

### Plural verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#use-plural)

Bei SAP existiert eine veraltete Praxis, Tabellen im Singular zu benennen, z.B. `country` im Falle einer Ländertabelle. Die allgemeine Tendenz außerhalb von SAP ist jedoch, für Auflistungen den Plural zu verwenden. Wir empfehlen daher, stattdessen `countries` zu bevorzugen.

> Diese Empfehlung betrifft hauptsächlich Dinge wie Variablen und Eigenschaften. > Für Entwicklungsobjekte können konkurrierende Muster gültig sein, > die ebenfalls sinnvoll sind, z.B. die weit verbreitete Konvention > zur Benennung von Datenbanktabellen („transparente Tabellen“) im Singular.

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Zweckbeschreibende Namen wählen_ in [Robert C. Martins _Clean Code_].

### Aussprechbare Namen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#use-pronounceable-names)

Wir denken und reden eine Menge über Objekte. Verwenden Sie daher Namen, die jeder aussprechen kann. Verwenden Sie z.B. eher `detection_object_types` als etwas Kryptisches wie `dobjt`.

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Aussprechbare Namen verwenden_ in [Robert C. Martins _Clean Code_].

### Abkürzungen vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#avoid-abbreviations)

Wenn Sie genügend Platz haben, schreiben Sie die Namen vollständig aus. Kürzen Sie nur ab, wenn Sie andernfalls die Längenbegrenzungen überschreiten.

Wenn eine Abkürzung unumgänglich ist, kürzen Sie zuerst die _unwichtigen_ Wörter.

Das Abkürzen von Wörtern mag auf den ersten Blick effizient erscheinen, kann jedoch schnell zu Missverständnissen führen. So ist z.B. unklar, ob sich „cust“ in `cust` auf „customizing“, „customer“ oder „custom“ bezieht. Alle drei Begriffe sind in SAP-Anwendungen geläufig.

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Unterschiede deutlich machen_ in [Robert C. Martins _Clean Code_].

### Abkürzungen konsistent verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#use-same-abbreviations-everywhere)

Bei der Suche nach dem relevanten Code werden Stichwörter verwendet. Unterstützen Sie dies, indem Sie für eine Sache immer dieselbe Abkürzung verwenden. Kürzen Sie z.B. „Aufdeckungsobjekttyp“ immer mit „AufdObTp“ ab, und nicht zusätzlich mit „AdObjTyp“, „AufdckObjtT“ usw.

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Suchbare Namen verwenden_ in [Robert C. Martins _Clean Code_].

### Nomen für Klassen, Verben für Methoden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#use-nouns-for-classes-and-verbs-for-methods)

Verwenden Sie Nomen oder Nominalphrasen zur Benennung von Klassen, Schnittstellen und Objekten:

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

Verwenden Sie Verben oder Verbphrasen zur Benennung von Methoden:

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

Verben am Anfang von booleschen Methoden wie `is_` und `has_` erzeugen einen angenehmen Lesefluss:

```ABAP
IF is_empty( table ).
```

Wir empfehlen, Funktionen wie Methoden zu benennen:

```ABAP
FUNCTION /clean/read_alerts
```

### Stoppwörter wie „Daten“, „Info“, „Objekt“ vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#avoid-noise-words-such-as-data-info-object)

Lassen Sie Stoppwörter weg

```ABAP
account  " instead of account_data
alert    " instead of alert_object
```

oder ersetzen Sie diese durch ein spezifisches Wort, das wirklich einen sinnvollen Zweck erfüllt

```ABAP
user_preferences          " instead of user_info
response_time_in_seconds  " instead of response_time_variable
```

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Unterschiede deutlich machen_ in [Robert C. Martins _Clean Code_].

### Ein Wort pro Konzept wählen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#pick-one-word-per-concept)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

Wählen Sie einen Begriff für ein Konzept und bleiben Sie dabei; verwenden Sie ihn nicht abwechselnd mit anderen Synonymen. Synonyme vergeuden die Zeit des Lesers, der versucht, einen nicht vorhandenen Unterschied herauszufinden.

```ABAP
" anti-pattern
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Wählen Sie ein Wort pro Konzept_ in [Robert C. Martins _Clean Code_]

### Musternamen nur mit Absicht verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#use-pattern-names-only-if-you-mean-them)

Verwenden Sie die Namen von Software-Entwurfsmustern nicht für Klassen und Schnittstellen, es sei denn, sie beziehen sich wirklich auf diese. Nennen Sie Ihre Klasse z.B. nicht `file_factory`, es sei denn, sie implementiert wirklich das factory-Entwurfsmuster. Zu den häufigsten Mustern zählen:[singleton](https://en.wikipedia.org/wiki/Singleton_pattern),
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern),
[facade](https://en.wikipedia.org/wiki/Facade_pattern),
[composite](https://en.wikipedia.org/wiki/Composite_pattern),
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern),
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern),
[observer](https://en.wikipedia.org/wiki/Observer_pattern) und [strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

> Mehr erfahren Sie in _Kapitel 2: Aussagekräftige Namen: Fehlinformationen vermeiden_ in [Robert C. Martins _Clean Code_].

### Codierungen vermeiden, insbes. Ungarische Notation und Präfixe

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Namen](#names) > [Dieser Abschnitt](#avoid-encodings-esp-hungarian-notation-and-prefixes)

Wir ermutigen Sie dazu, sich _aller_ Codierungs-Präfixe zu entledigen.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

Die Gründe hierfür beschreibt

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> [Avoid Encodings](sub-sections/AvoidEncodings.md)
>  ausführlicher.

## Sprache

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#language)

### Vorsicht bei älteren ABAP-Releases

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Sprache](#language) > [Dieser Abschnitt](#mind-the-legacy)

Wenn Sie für ältere ABAP-Releases codieren, befolgen Sie die Empfehlungen in diesem Leitfaden mit Bedacht. Viele der folgenden Empfehlungen nutzen relativ neue Syntax und Konstrukte, die in älteren ABAP-Releases möglicherweise nicht unterstützt werden. Prüfen Sie die Empfehlungen, die Sie umsetzen möchten, am ältesten Release, das Sie unterstützen müssen. Lehnen Sie Clean Code nicht einfach als Ganzes ab. Der größte Teil der Regeln (z.B. Namen, Kommentare) funktioniert mit _jeder_ ABAP-Version.

### Performance beachten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Sprache](#language) > [Dieser Abschnitt](#mind-the-performance)

Wenn Sie High-Performance-Komponenten codieren, setzen Sie die Empfehlungen in diesem Leitfaden mit Bedacht um. Einige Aspekte des Clean Code können die Performance bremsen (mehr Methodenaufrufe), oder mehr Speicher verbrauchen (mehr Objekte). ABAP weist einige Besonderheiten auf, die diesen Effekt verstärken können. So vergleicht ABAP beim Aufruf einer Methode beispielsweise die Datentypen. Dies kann dazu führen, dass z.B. das Aufteilen einer einzelnen umfangreichen Methode in mehrere Sub-Methoden zu einer verlangsamten Code-Verarbeitung führt.

Wir empfehlen jedoch dringend, nicht aufgrund unklarer Befürchtungen voreilig zu optimieren. Die überwiegende Mehrheit der Regeln (z.B. Namen, Kommentare) hat keine negativen Auswirkungen zur Folge. Versuchen Sie, beim Aufbau auf saubere, objektorientierte Weise vorzugehen. Ist Ihnen etwas zu langsam, nehmen Sie eine Performance-Messung vor. Erst dann sollten Sie anhand der Fakten über die Verwerfung ausgewählter Regeln entscheiden.

Einige weitere Gedanken, teilweise dem Kapitel 2 von [Martin Fowler_Refactoring_](https://martinfowler.com/books/refactoring.html) entnommen:

In einer typischen Anwendung wird die meiste Laufzeit in einem sehr kleinen Teil des Codes verbraucht. Gerade mal 10 % des Codes können 90 % der Laufzeit beanspruchen, und insbesondere in ABAP ist ein großer Anteil der Laufzeit reine Datenbankzeit.

Daher ist es kein optimaler Ressourceneinsatz, einen erheblichen Aufwand darauf zu verwenden, _den gesamten Code_ zu jeder Zeit hocheffizient zu machen. Damit soll nicht gesagt werden, dass Sie das Laufzeitverhalten ignorieren sollen. Legen Sie während der initialen Entwicklung mehr Fokus auf einen sauberen, gut strukturierten Code, und nutzen Sie den Profiler zur Identifizierung von kritischen Bereichen, die eine Optimierung erfordern.

Wir gehen so weit zu behaupten, dass eine solche Vorgehensweise eine positive Nettowirkung auf die Performance hat, da es sich um einen gezielteren Optimierungsaufwand handelt, und es in einem gut strukturierten Code einfacher sein sollte, Performance-Engpässe zu identifizieren, zu refaktorieren und zu optimieren.

### Besser Objektorientierung als prozedurale Programmierung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Sprache](#language) > [Dieser Abschnitt](#prefer-object-orientation-to-procedural-programming)

Objektorientierte Programme (Klassen, Schnittstellen) sind besser segmentiert und können einfacher refaktoriert und getestet werden als prozeduraler Code (Funktionen, Programme). Auch wenn es Situationen gibt, in denen Sie prozedurale Objekte bereitstellen müssen (eine Funktion für einen RFC, ein Programm für eine Transaktion), sollten diese Objekte weniger mehr tun als eine entsprechende Klasse aufzurufen, die die tatsächliche Funktion bereitstellt:

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> [Funktionsgruppen vs. Klassen](sub-sections/FunctionGroupsVsClasses.md) > beschreibt die Unterschiede im Detail.

### Besser funktionale als prozedurale Sprachkonstruktion

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Sprache](#language) > [Dieser Abschnitt](#prefer-functional-to-procedural-language-constructs)

Sie sind gewöhnlich kürzer und den modernen Programmierern geläufiger.

```ABAP
DATA(variable) = 'A'.
" MOVE 'A' TO variable.

DATA(uppercase) = to_upper( lowercase ).
" TRANSLATE lowercase TO UPPER CASE.

index += 1.         " >= NW 7.54
index = index + 1.  " < NW 7.54
" ADD 1 TO index.

DATA(object) = NEW /clean/my_class( ).
" CREATE OBJECT object TYPE /dirty/my_class.

result = VALUE #( FOR row IN input ( row-text ) ).
" LOOP AT input INTO DATA(row).
"  INSERT row-text INTO TABLE result.
" ENDLOOP.

DATA(line) = value_pairs[ name = 'A' ].
" READ TABLE value_pairs INTO DATA(line) WITH KEY name = 'A'.

DATA(exists) = xsdbool( line_exists( value_pairs[ name = 'A' ] ) ).
IF line_exists( value_pairs[ name = 'A' ] ).
" READ TABLE value_pairs TRANSPORTING NO FIELDS WITH KEY name = 'A'.
" DATA(exists) = xsdbool( sy-subrc = 0 ).
```

Viele der im Folgenden angeführten Detailregeln sind lediglich spezifischere Wiederholungen dieser allgemeinen Empfehlung.

### Obsolete Sprachelemente vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Sprache](#language) > [Dieser Abschnitt](#avoid-obsolete-language-elements)

Achten Sie beim Upgrade Ihrer ABAP-Version auf obsolete Sprachelemente und verzichten Sie auf deren weitere Verwendung.

So machen es beispielsweise die `@`-escaped „host“-Variablen in der folgenden Anweisung etwas klarer, was eine Programmvariable ist, und was eine Spalte in der Datenbank,

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

im Vergleich zur [obsoleten Form ohne escape-Zeichen](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm)

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

Neuere Alternativen verbessern mittels moderner Programmierparadigmen die Lesbarkeit des Codes und reduzieren Design-Konflikte, so dass ein Wechsel zu diesen Alternativen automatisch Ihren Code bereinigen kann.

Bei weiterer Verwendung profitieren obsolete Elemente möglicherweise im Hinblick auf Verarbeitungsgeschwindigkeit und Speicherverbrauch nicht mehr von Optimierungsmaßnahmen.

Moderne Sprachelemente tragen außerdem dazu bei, Ihre jüngeren ABAP-Spezialisten an Bort zu holen, denen die veralteten Konstrukte möglicherweise fremd sind, weil diese nicht mehr in den SAP-Schulungen vermittelt werden.

Die SAP NetWeaver-Dokumentation enthält einen Abschnitt, der obsolete Sprachelemente aufführt, z.B.
[NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm).

### Entwurfsmuster mit Bedacht einsetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Sprache](#language) > [Dieser Abschnitt](#use-design-patterns-wisely)

Verwenden Sie Entwurfsmuster, wo sie geeignet sind und deutliche Vorteile mit sich bringen. Wenden Sie Entwurfsmuster nicht einfach beliebig an jeder Stelle an.

## Konstanten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#constants)

### Konstanten statt magischer Zahlen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Konstanten](#constants) > [Dieser Abschnitt](#use-constants-instead-of-magic-numbers)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

ist klarer als

```ABAP
" anti-pattern
IF abap_type = 'D'.
```

> Mehr erfahren Sie in _Kapitel 17: Smells und Heuristiken: G25:
> Magische Zahlen durch benannte Konstanten ersetzen_ in [Robert C. Martins _Clean Code_].

### Besser Enumerationsklassen als Konstanten-Interfaces

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Konstanten](#constants) > [Dieser Abschnitt](#prefer-enumeration-classes-to-constants-interfaces)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

oder

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

anstatt Dinge zu vermischen, die nichts miteinander zu tun haben

```ABAP
" anti-pattern
INTERFACE /dirty/common_constants.
  CONSTANTS:
    warning      TYPE symsgty VALUE 'W',
    transitional TYPE i       VALUE 1,
    error        TYPE symsgty VALUE 'E',
    persisted    TYPE i       VALUE 2.
ENDINTERFACE.
```

> [Enumerationen](sub-sections/Enumerations.md)
> beschreibt allgemeine Enumerationsmuster
> und erläutert ihre Vor- und Nachteile.
> 
> Mehr erfahren Sie in _Kapitel 17: Smells und Heuristiken: J3: Konstanten im Gegensatz zu Enums_ in [Robert C. Martins _Clean Code_].

### Konstanten gruppieren, wenn Sie auf Enumerationsklassen verzichten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Konstanten](#constants) > [Dieser Abschnitt](#if-you-dont-use-enumeration-classes-group-your-constants)

Wenn Sie Konstanten lose sammeln, beispielsweise in einer Schnittstelle, gruppieren Sie diese:

```ABAP
CONSTANTS:
  BEGIN OF message_severity,
    warning TYPE symsgty VALUE 'W',
    error   TYPE symsgty VALUE 'E',
  END OF message_severity,
  BEGIN OF message_lifespan,
    transitional TYPE i VALUE 1,
    persisted    TYPE i VALUE 2,
  END OF message_lifespan.
```

Macht die Beziehung klarer als:

```ABAP
" Anti-pattern
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

Die Gruppe erlaubt Ihnen außerdem einen gruppenweisen Zugriff, z.B. zur Eingabeprüfung:

```ABAP
DO number_of_constants TIMES.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(<constant>).
  IF <constant> = input.
    is_valid = abap_true.
    RETURN.
  ENDIF.
ENDWHILE.
```

> Mehr erfahren Sie in _Kapitel 17: Smells und Heuristiken: G27: Struktur ist wichtiger als Konvention_ in [Robert C. Martins _Clean Code_].

## Variablen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#variables)

### Besser Inline-Deklaration als voranstehende Deklaration

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Variablen](#variables) > [Dieser Abschnitt](#prefer-inline-to-up-front-declarations)

Wenn Sie diese Empfehlungen befolgen, werden Ihre Methoden so kurz (3-5 Anweisungen), dass die Inline-Deklaration der Variablen bei ihrem ersten Auftreten natürlicher erscheint

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

als die Deklaration der Variablen mit einem `DATA`-Abschnitt am Anfang der Methode

```ABAP
" anti-pattern
METHOD do_something.
  DATA:
    name   TYPE seoclsname,
    reader TYPE REF TO /dirty/reader.
  name = 'something'.
  reader = /dirty/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

> Mehr erfahren Sie in _Kapitel 5: Formatierung: Vertikaler Abstand_ in [Robert C. Martins _Clean Code_].

### Keine Inline-Deklaration in optionalen Verzweigungen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Variablen](#variables) > [Dieser Abschnitt](#dont-declare-inline-in-optional-branches)

```ABAP
" anti-pattern
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

Das funktioniert gut, weil ABAP Inline-Deklarationen so behandelt, als stünden sie am Anfang der Methode. Es ist jedoch extrem verwirrend für die Leser, insbesondere, wenn die Methode länger und die Deklaration nicht auf den ersten Blick zu erkennen ist. Weichen Sie in diesem Fall von der Inline-Deklaration ab und stellen Sie die Deklaration an den Anfang:

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> Mehr erfahren Sie in _Kapitel 5: Formatierung: Vertikaler Abstand_ in [Robert C. Martins _Clean Code_].

### Keine Verkettung von voranstehenden Deklarationen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Variablen](#variables) > [Dieser Abschnitt](#do-not-chain-up-front-declarations)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /dirty/reader.
```

Verkettung suggeriert, dass die definierten Variablen auf logischer Ebene zusammengehören. Im Sinne einer konsistenten Verwendung müssten Sie sicherstellen, dass alle verketteten Variablen zusammengehören, und zusätzliche Verkettungsgruppen zum Hinzufügen von Variablen einführen. Obwohl dies möglich ist, lohnt sich der Aufwand in der Regel nicht.

Verkettung macht darüber hinaus das Neuformatieren und Refactoring unnötig kompliziert, da jede Zeile anders aussieht und sie sich bei jeder Änderung mit Doppelpunkten, Punkten und Kommas abplagen müssen - ein ungerechtfertigter Aufwand.

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO /dirty/reader.
```

> Lesen Sie hierzu auch [Type-Klauseln nicht ausrichten](#dont-align-type-clauses)> Wenn Sie die Verkettung von Datendeklarationen nutzen, verwenden Sie eine Kette für jede Gruppe zusammengehöriger Variablen.

### Besser REF TO als FIELD-SYMBOL

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Variablen](#variables) > [Dieser Abschnitt](#prefer-ref-to-to-field-symbol)

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

anstelle des Äquivalents

```ABAP
" anti-pattern
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

außer, wenn Sie Feldsymbole benötigen

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

Code-Prüfungen beweisen, dass Programmierer gerne Beides willkürlich einsetzen, „einfach so“, „weil wir LOOPs immer so definieren“ oder „aus keinem besonderen Grund“. Eine willkürliche Wahl führt jedoch dazu, dass der Leser wertvolle Zeit über der nutzlosen Frage vergeudet, warum das Eine und nicht das Andere verwendet wird. Daher sollten hier gut begründete, präzise Entscheidungen erfolgen. Unsere Empfehlung basiert auf dieser Begründung:

- Feldsymbole können einige Dinge, die Referenzen nicht können, wie z.B. dynamisch auf die Komponenten einer Struktur zugreifen. Entsprechend können Referenzen einige Dinge, die Feldsymbole nicht können, wie z.B. eine dynamische Datenstruktur aufbauen. Zusammenfassend ist es nicht möglich, sich ausschließlich für eine der beiden Optionen zu entscheiden.

- Im objektorientierten ABAP sind Referenzen allgegenwärtig und unvermeidlich, da jedes Objekt ein `REF TO <class-name>` ist. Im Gegensatz dazu sind Feldsymbole nur in wenigen, die dynamische Typisierung betreffenden Sonderfällen strikt erforderlich. Referenzen sind daher in jedem objektorientierten Programm die natürliche Präferenz.

- Feldsymbole sind kürzer als Referenzen. Die daraus resultierende Speicherersparnis ist jedoch so gering, dass sie getrost vernachlässigt werden kann. Ähnlich ist auch die Geschwindigkeit kein Thema. Folglich gibt es aus Performance-Perspektive keinen Grund, das Eine oder das Andere zu bevorzugen.

> Mehr erfahren Sie im Artikel > [_Dynamischer Zugriff auf Datenobjekte_ in den ABAP-Programmierrichtlinien](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm).

## Tabellen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#tables)

### Korrekte Tabellenart verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#use-the-right-table-type)

- Sie verwenden `HASHED`-Tabellen in der Regen für **große Tabellen**, die in einem **einzigen Schritt befüllt**, **nie modifiziert** und **häufig anhand ihres Schlüssels gelesen** werden. Ihr inhärenter Speicher- und Verarbeitungsaufwand macht Hash-Tabellen nur bei großen Datenmengen und häufigen Lesezugriffen sinnvoll. Jede Änderung des Tabelleninhalts erfordert eine kostenintensive Hash-Neuberechnung. Somit ist diese Tabellenart ungeeignet für Tabellen, die häufig geändert werden.

- `SORTED`-Tabellen nutzen Sie üblicherweise für **umfangreiche Tabellen**, die **nach und nach gefüllt werden**, **ständig sortiert** oder **modifiziert werden müssen** und **häufig anhand eines oder mehrerer Ganz- oder Teilschlüssel gelesen** oder **in einer bestimmten Reihenfolge** verarbeitet werden.
Das Hinzufügen, Ändern oder Löschen von Inhalt setzt voraus, dass zunächst die richtige Stelle zum Einfügen gefunden wird, jedoch muss der Rest des Tabellenindizes nicht angepasst werden. Sortierte Tabellen erweisen sich nur bei einer großen Anzahl von Lesezugriffen als wertvoll.

- Verwenden Sie `STANDARD`-Tabellen für **kleine Tabellen**, deren Indizierung mehr Aufwand als Nutzen erzeugt, und **Arrays**, bei denen die Reihenfolge der Zeilen entweder überhaupt keine Rolle spielt, oder die Sie genau in der Reihenfolge verarbeiten möchten, in der sie angehängt sind. Diese Tabellen sind auch dann geeignet, wenn unterschiedliche Tabellenzugriffe erforderlich sind, z.B. indizierter Zugriff und sortierter Zugriff mit `SORT` und `BINARY SEARCH`.

> Dies sind lediglich grobe Richtlinien.
> Mehr Informationen enthält der Artikel [_Auswahl der Tabellenart_ in der ABAP-Hilfe](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm).

### DEFAULT KEY vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#avoid-default-key)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

Standardschlüssel werden häufig nur hinzugefügt, um die neueren funktionalen Anweisungen zum Laufen zu bringen. Die Schlüssel selbst sind tatsächlich in der Regel überflüssig und verschwenden unnötig Ressourcen. Sie können sogar zu unklaren Fehlern führen, da sie numerische Datentypen ignorieren. Die Anweisungen `SORT` und `DELETE ADJACENT` greifen ohne explizite Feldliste auf den Primärschlüssel der internen Tabelle zu, was im Falle der Verwendung von `DEFAULT KEY` zu sehr unerwarteten Ergebnissen führen kann, wenn z.B. numerische Felder als Komponente des Schlüssels verwendet werden, insbesondere in Kombination mit `READ TABLE ... BINARY` usw.

Geben Sie die Schlüsselkomponenten entweder explizit an,

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

oder greifen Sie auf `EMPTY KEY` zurück, wenn Sie keinen Schlüssel benötigen.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> Gemäß [Blog von Horst Keller _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)
> **Vorsicht:** `SORT` in internen Tabellen mit `EMPTY KEY` hat keine Sortierung zur Folge, aber es werden > Syntaxwarnungen abgesetzt, falls statistisch bestimmt werden kann, dass der Schlüssel leer ist.

### Besser INSERT INTO TABLE als APPEND TO

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#prefer-insert-into-table-to-append-to)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` funktioniert mit allen Tabellenarten und Schlüsseltypen und macht es Ihnen somit einfacher, die Tabellenart und Schlüsseldefinitionen zu refaktorieren, wenn sich Ihre Performance-Anforderungen ändern.

Verwenden Sie `APPEND TO` nur, wenn Sie eine `STANDARD`-Tabelle in einer Array-ähnlichen Weise verwenden und hervorheben möchten, dass der hinzugefügte Eintrag die letzte Zeile sein soll.

### Besser LINE_EXISTS als READ TABLE oder LOOP AT

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#prefer-line_exists-to-read-table-or-loop-at)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

drückt die Absicht klarer und kürzer aus als

```ABAP
" anti-pattern
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

oder sogar

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Besser READ TABLE als LOOP AT

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#prefer-read-table-to-loop-at)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

drückt die Absicht klarer und kürzer aus als

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

oder sogar

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Besser LOOP AT WHERE als verschachteltes IF

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#prefer-loop-at-where-to-nested-if)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

drückt die Absicht klarer und kürzer aus als

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Überflüssige Lesezugriffe auf Tabelle vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Tabellen](#tables) > [Dieser Abschnitt](#avoid-unnecessary-table-reads)

Sofern Sie hier eine Zeile _erwarten_, verwenden Sie einen Lesevorgang und reagieren auf die Ausnahme,

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

anstatt den Haupt-Kontrollfluss durch einen doppelten Lesevorgang zu „verunreinigen“ und zu verlangsamen.

```ABAP
" anti-pattern
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
DATA(row) = my_table[ key = input ].
```

> Neben einer Performance-Verbesserung ist dies außerdem eine spezielle Variante des allgemeiner formulierten Prinzips [Konzentrieren Sie sich auf den glücklichen Pfad ODER die Fehlerbehebung](#focus-on-the-happy-path-or-error-handling-but-not-both).

## Strings

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#strings)

### Literale mit ` definieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Strings](#strings) > [Dieser Abschnitt](#use--to-define-literals)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

Verzichten Sie auf die Verwendung von `'`, da dies eine überflüssige Typkonvertierung hinzufügt und den Leser darüber im Unklaren lässt, ob er es mit einem `CHAR` oder `STRING` zu tun hat:

```ABAP
" anti-pattern
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` ist im Allgemeinen in Ordnung, kann jedoch nicht für `CONSTANTS` verwendet werden und führt bei der Angabe eines Festwertes zu unnötigem Mehraufwand:

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### Text mit | assemblieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Strings](#strings) > [Dieser Abschnitt](#use--to-assemble-text)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

String-Vorlagen heben besser hervor, was literal und was variabel ist, insbesondere, wenn Sie mehrere Variablen in einem Text einbetten.

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Boolesche Ausdrücke

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#booleans)

### Boolesche Ausdrücke mit Bedacht einsetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Boolesche Ausdrücke](#booleans) > [Dieser Abschnitt](#use-booleans-wisely)

Wir treffen häufig auf Fälle, in denen boolesche Ausdrücke die natürliche Wahl zu sein scheinen,

```ABAP
" anti-pattern
is_archived = abap_true.
```

bis eine Änderung der Perspektive nahelegt, dass eine Enumeration besser gewesen wäre

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

Im Allgemeinen sind boolesche Ausdrücke eine ungeeignete Wahl zur Unterscheidung der Typen von verschiedenen Dingen, da Sie fast immer auf Fälle stoßen werden, die nicht ausschließlich das Eine oder das Andere sind.

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

[Aufgeteilte Methode statt boolescher Eingabeparameter](#split-method-instead-of-boolean-input-parameter) erläutert, warum Sie boolesche Parameter immer hinterfragen sollten.

> Mehr zu diesem Thema erfahren Sie in [1](http://www.beyondcode.org/articles/booleanVariables.html)

### ABAP_BOOL für boolesche Ausdrücke verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Boolesche Ausdrücke](#booleans) > [Dieser Abschnitt](#use-abap_bool-for-booleans)

```ABAP
DATA has_entries TYPE abap_bool.
```

Verwenden Sie nicht den generischen Typ `char1`. Obwohl er aus technischer Sicht kompatibel ist, verschleiert er die Tatsache, dass wir es mit einer booleschen Variable zu tun haben.

Vermeiden Sie auch andere boolesche Typen, da diese häufig seltsame Nebeneffekte haben. So unterstützt `boolean` beispielsweise einen dritten Wert „undefined“, der zu subtilen Programmierfehlern führt.

In einigen Fällen benötigen Sie möglicherweise ein Data-Dictionary-Element, z.B. für Dynpro-Felder. `abap_bool` kann hier nicht verwendet werden, da es im Typpool `abap` definiert wird, nicht im Data Dictionary. Greifen Sie in diesem Fall auf `boole_d` oder `xfeld` zurück. Erzeugen Sie Ihr eigenes Datenelement, wenn Sie eine spezifische Beschreibung benötigen.

> ABAP ist möglicherweise die einzige Programmiersprache, die keinen universellen booleschen Datentyp besitzt. Ein boolescher Datentyp ist jedoch zwingend erforderlich. Diese Empfehlung basiert auf den ABAP-Programmierrichtlinien.

### ABAP_TRUE und ABAP_FALSE für Vergleiche verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Boolesche Ausdrücke](#booleans) > [Dieser Abschnitt](#use-abap_true-and-abap_false-for-comparisons)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

Verwenden Sie nicht die Zeichenäquivalente `'X'` und `' '` oder `space`. Sie machen es schwer erkennbar, dass dies ein boolescher Ausdruck ist:

```ABAP
" anti-pattern
has_entries = 'X'.
IF has_entries = space.
```

Vermeiden Sie Vergleiche mit `INITIAL` - der Leser muss sich dann daran erinnern, dass `abap_bool` den Standardwert `abap_false` hat:

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> ABAP ist möglicherweise die einzige Programmiersprache, die keine integrierten Konstanten für wahr und falsch besitzt. Diese sind jedoch zwingend erforderlich. Diese Empfehlung basiert auf den ABAP-Programmierrichtlinien.

### XSDBOOL für boolesche Variablen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Boolesche Ausdrücke](#booleans) > [Dieser Abschnitt](#use-xsdbool-to-set-boolean-variables)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

Das Äquivalent `IF`-`THEN`-`ELSE` ist viel länger, ohne einen Vorteil zu erbringen:

```ABAP
" anti-pattern
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool` ist die beste Methode für unseren Zweck, da sie direkt ein `char1` produziert, das am Besten zu unserem booleschen Typ `abap_bool` passt. Die äquivalenten Funktionen `boolc` und `boolx` erzeugen andere Typen und führen darüber hinaus zu einer überflüssigen, impliziten Typkonvertierung.

Wir stimmen damit überein, dass der Name `xsdbool` unglücklich und irreführend ist - schließlich sind wir überhaupt nicht an den XML-Schemadefinition interessiert, die das Präfix „xsd“ nahelegt.

Eine mögliche Alternative zu `xsdbool` ist die `COND`-Dreifach-Form. Ihre Syntax ist intuitiv, aber ein bisschen länger, weil sie überflüssigerweise das Segment `THEN abap_true` wiederholt und Kenntnis des impliziten Standardwerts `abap_false` erfordert. Daher schlagen wir diese Form nur als Zweitlösung vor.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## Bedingungen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#conditions)

### Bedingungen nach Möglichkeit positiv definieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Bedingungen](#conditions) > [Dieser Abschnitt](#try-to-make-conditions-positive)

```ABAP
IF has_entries = abap_true.
```

Sehen Sie zum Vergleich, wie schwer verständlich dieselbe Anweisung durch Umkehr wird:

```ABAP
" anti-pattern
IF has_no_entries = abap_false.
```

Der Hinweis „nach Möglichkeit“ im Abschnittstitel bedeutet, dass Sie dies nicht bis zu dem Punkt erzwingen sollten, wo sie mit Dingen wie [leeren IF-Verzweigungen](#no-empty-if-branches) enden.

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> Mehr erfahren Sie in _Kapitel 17: Smells und Heuristiken: G29: Negative Bedingungen vermeiden_ von [Robert C. Martins _Clean Code_].

### Besser IS NOT als NOT IS

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Bedingungen](#conditions) > [Dieser Abschnitt](#prefer-is-not-to-not-is)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

Verneinung ist logisch äquivalent, erfordert jedoch eine mentale Umkehrung, die sie schwieriger verständlich macht.

```ABAP
" anti-pattern
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> Eine spezifischere Variante von [Bedingungen nach Möglichkeit positiv definieren](#try-to-make-conditions-positive). Dies wird auch in Abschnitt [Alternative Sprachkonstrukte](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenalternative_langu_guidl.htm)
in den ABAP-Programmierrichtlinien beschrieben.

### Komplexe Bedingungen zerlegen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Bedingungen](#conditions) > [Dieser Abschnitt](#consider-decomposing-complex-conditions)

Bedingungen können einfacher werden, wenn Sie diese in die elementaren Bestandteile zerlegen, aus denen sie sich zusammensetzen:

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

anstatt alles zusammen:

```ABAP
" anti-pattern
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> Verwenden Sie die Quick Fixes der ABAP Development Tools, um Bedingungen schnell zu extrahieren und Variablen zu erzeugen (siehe oben).

### Komplexe Bedingungen extrahieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Bedingungen](#conditions) > [Dieser Abschnitt](#consider-extracting-complex-conditions)

Es ist fast immer eine gute Idee, komplexe Bedingungen in eigene Methoden zu extrahieren:

```ABAP
IF is_provided( example ).

METHOD is_provided.
  DATA(is_filled) = xsdbool( example IS NOT INITIAL ).
  DATA(is_working) = xsdbool( applies( example ) = abap_true OR
                              fits( example ) = abap_true ).
  result = xsdbool( is_filled = abap_true AND
                    is_working = abap_true ).
ENDMETHOD.
```

## IF

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#ifs)

### Keine leeren IF-Verzweigungen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [IF](#ifs) > [Dieser Abschnitt](#no-empty-if-branches)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

ist kürzer und klarer als

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### Bei mehreren Alternativbedingungen besser CASE als ELSE IF

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [IF](#ifs) > [Dieser Abschnitt](#prefer-case-to-else-if-for-multiple-alternative-conditions)

```ABAP
CASE type.
  WHEN type-some_type.
    " ...
  WHEN type-some_other_type.
    " ...
  WHEN OTHERS.
    RAISE EXCEPTION NEW /clean/unknown_type_failure( ).
ENDCASE.
```

`CASE` macht es einfach, eine Reihe von Alternativen zu sehen, die einander ausschließen, und kann schneller sein als eine Reihe von `IF`s, weil es in einen anderen Mikroprozessorbefehl umgesetzt werden kann, anstatt in eine Reihe von nacheinander ausgewerteten Bedingungen. Sie können neue Fälle schnell einführen, ohne die betreffende Variable wiederholen zu müssen. Die Anweisung verhindert sogar einige Fehler, die auftreten, wenn die `IF`-`ELSEIF`s versehentlich verschachtelt werden.

```ABAP
" anti-pattern
IF type = type-some_type.
  " ...
ELSEIF type = type-some_other_type.
  " ...
ELSE.
  RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

### Schachtelungstiefe so gering wie möglich halten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [IF](#ifs) > [Dieser Abschnitt](#keep-the-nesting-depth-low)

```ABAP
" ani-pattern
IF <this>.
  IF <that>.
  ENDIF.
ELSE.
  IF <other>.
  ELSE.
    IF <something>.
    ENDIF.
  ENDIF.
ENDIF.
```

Verschachtelte `IF`s werden sehr schnell unverständlich und erfordern eine überproportionale Anzahl von Testfällen für ihre vollständige Abdeckung.

Entscheidungsbäume können gewöhnlich durch die Erzeugung von Submethoden und die Einführung von booleschen Hilfsvariablen aufgesplittet werden.

Andere Fälle können durch das Zusammenführen von IFs vereinfacht werden, wie z.B.

```ABAP
IF <this> AND <that>.
```

anstelle der unnötigen Verschachtelung

```ABAP
" anti-pattern
IF <this>.
  IF <that>.
```

## Reguläre Ausdrücke

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#regular-expressions)

### Besser einfachere Methoden als reguläre Ausdrücke

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Reguläre Ausdrücke](#regular-expressions) > [Dieser Abschnitt](#prefer-simpler-methods-to-regular-expressions)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

Reguläre Ausdrücke werden sehr schnell unverständlich. Einfache Fälle sind in der Regel problemloser, wenn auf reguläre Ausdrücke verzichtet wird.

Reguläre Ausdrücke verbrauchen außerdem in der Regel mehr Speicher und Verarbeitungszeit, da sie in einen Ausdrucksbaum geparst und zur Laufzeit in einen ausführbaren Matcher kompiliert werden müssen. Einfache Lösungen kommen evtl. mit einer simplen Loop und einer temporären Variable aus.

### Besser Basisprüfungen als reguläre Ausdrücke

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Reguläre Ausdrücke](#regular-expressions) > [Dieser Abschnitt](#prefer-basis-checks-to-regular-expressions)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

anstatt das Rad neu zu erfinden

```ABAP
" anti-pattern
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> Es gibt anscheinend eine natürliche Tendenz zur Blindheit für das Don't-Repeat-Yourself(DRY)-Prinzip („wiederhole dich nicht“), wenn es um reguläre Ausdrücke geht. Siehe hierzu Abschnitt _Kapitel 17: Smells und Heuristiken: Allgemein: G5: Duplizierung_ in [Robert C. Martins _Clean Code_].

### Komplexe reguläre Ausdrücke assemblieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Reguläre Ausdrücke](#regular-expressions) > [Dieser Abschnitt](#consider-assembling-complex-regular-expressions)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

Manche komplexe reguläre Ausdrücke werden einfacher verständlich, wenn Sie dem Leser zeigen, wie diese sich aus elementaren Bestandteilen zusammensetzen.

## Klassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#classes)

### Klassen: Objektorientierung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Dieser Abschnitt](#classes-object-orientation)

#### Besser Objekte als statische Klassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Klassen: Objektorientierung](#classes-object-orientation) > [Dieser Abschnitt](#prefer-objects-to-static-classes)

Statische Klassen verzichten zunächst einmal auf alle Vorteile, die durch die Objektorientierung erzielt wurden. Sie machen es insbesondere fast unmöglich, produktive Abhängigkeiten in Modultests durch Testattrappen zu ersetzen.

Wenn Sie darüber nachdenken, ob Sie eine Klasse oder Methode statisch machen sollten, lautet die Antwort fast immer: Nein.

Eine akzeptierte Ausnahme zu dieser Regel sind einfache utils-Klassen. Ihre Methoden machen es einfacher, mit bestimmten ABAP-Typen zu interagieren. Sie sind nicht nur vollständig stateless, sondern auch so simpel, dass sie wie ABAP-Anweisungen oder integrierte Funktionen aussehen. Der unterscheidende Faktor ist, dass ihre Konsumenten diese Klassen so eng in ihren Code einbinden, dass sie diese nicht in Modultests nachstellen möchten.

```ABAP
CLASS /clean/string_utils DEFINITION [...].
  CLASS-METHODS trim
   IMPORTING
     string        TYPE string
   RETURNING
     VALUE(result) TYPE string.
ENDCLASS.

METHOD retrieve.
  DATA(trimmed_name) = /clean/string_utils=>trim( name ).
  result = read( trimmed_name ).
ENDMETHOD.
```

#### Besser Komposition als Vererbung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Klassen: Objektorientierung](#classes-object-orientation) > [Dieser Abschnitt](#prefer-composition-to-inheritance)

Vermeiden Sie den Aufbau von Klassenhierarchien mit Vererbung. Bevorzugen Sie stattdessen Komposition.

Das Design einer sauberen Vererbung ist schwierig, da Sie Regeln beachten müssen, wie z.B. das [Liskovsche Substitutionsprinzip](https://en.wikipedia.org/wiki/Liskov_substitution_principle).
Sie ist außerdem schwer verständlich, weil hierzu die Grundprinzipien hinter der Hierarchie realisiert und verstanden sein müssen.
Vererbung reduziert die Wiederverwendung, weil die Methoden tendenziell nur den Subklassen verfügbar gemacht werden.
Sie macht außerdem das Refactoring komplizierter, da wechselnde oder veränderte Mitglieder oftmals Änderungen am gesamten Hierarchiebaum erfordern.

Komposition bedeutet, dass Sie kleine, unabhängige Objekte entwerfen, von denen jedes einem bestimmten Zweck dient.
Diese Objekte können mittels einfacher Delegations- und Fassadenmuster in komplexeren Objekten neu kombiniert werden.
Komposition kann zur Erzeugung von mehr Klassen führen, hat jedoch sonst keine weiteren Nachteile.

Lassen Sie sich von dieser Regel nicht entmutigen, die Vererbung an der richtigen Stelle zu verwenden.
Es gibt gute Anwendungsmöglichkeiten für die Vererbung, wie z.B.
das [Composite Design Pattern](https://en.wikipedia.org/wiki/Composite_pattern).
Fragen Sie sich einfach nur kritisch, ob die Vererbung in Ihrem Fall wirklich mehr Vorteile als Nachteile mit sich bringen wird.
Wenn Sie Zweifel haben, ist die Komposition im Allgemeinen die sicherere Wahl.

> Das Dokument [Interfaces vs. Abstract Classes](sub-sections/InterfacesVsAbstractClasses.md)
vergleicht einige Details.

#### Kein Mix von Stateful und Stateless in derselben Klasse

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Klassen: Objektorientierung](#classes-object-orientation)

Vermischen Sie die Stateless- und Stateful-Programmiermodelle nicht in derselben Klasse.

In der Stateless-Programmierung erhalten Methoden Eingabe und produzieren Ausgabe,
_ohne jegliche Nebeneffekte_, und resultieren in Methoden, die dasselbe Ergebnis produzieren, unabhängig davon, wann und in welcher Reihenfolge sie aufgerufen werden.

```ABAP
CLASS /clean/xml_converter DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS convert
      IMPORTING
        file_content  TYPE xstring
      RETURNING
        VALUE(result) TYPE /clean/some_inbound_message.
ENDCLASS.

CLASS /clean/xml_converter IMPLEMENTATION.
  METHOD convert.
    cl_proxy_xml_transform=>xml_xstring_to_abap(
      EXPORTING
        xml       = file_content
        ext_xml   = abap_true
        svar_name = 'ROOT_NODE'
      IMPORTING
        abap_data = result ).
   ENDMETHOD.
ENDCLASS.
```

In der Stateful-Programmierung manipulieren wir den internen Zustand von Objekten durch ihre Methoden. Das heißt, sie ist _voller Nebeneffekte_.

```ABAP
CLASS /clean/log DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS add_message IMPORTING message TYPE /clean/message.
  PRIVATE SECTION.
    DATA messages TYPE /clean/message_table.
ENDCLASS.

CLASS /clean/log IMPLEMENTATION.
  METHOD add_message.
    INSERT message INTO TABLE messages.
  ENDMETHOD.
ENDCLASS.
```

Beide Modelle sind in Ordnung und haben ihre Anwendungsmöglichkeiten.
Wenn Sie diese Modelle jedoch im selben Objekt _mixen_, wird Code erzeugt, der schwer verständlich ist und garantiert mit unklaren Übertragungsfehlern und Synchronitätsproblemen fehlschlägt. Hiervon wird abgeraten.

### Scope

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Dieser Abschnitt](#scope)

#### Global ist Standard, lokal nur im Bedarfsfall

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Scope](#scope) > [Dieser Abschnitt](#global-by-default-local-only-where-appropriate)

Arbeiten Sie standardmäßig mit globalen Klassen. Verwenden Sie lokale Klassen nur wo geeignet.

> Globale Klassen sind die Klassen, die im Data Dictionary sichtbar sind.
Lokale Klassen existieren innerhalb des Includes eines anderen Entwicklungsobjekts und sind nur für dieses andere Objekt sichtbar.

Lokale Klassen eignen sich

- für sehr spezifische private Datenstrukturen, z.B. einen Iterator für die Daten der globalen Klasse, die immer nur hier benötigt werden,

- um einen komplexen privaten Algorithmus zu extrahieren, z.B. zur Trennung dieses speziellen Multi-Methoden-Sortieraggregat-Algorithmus vom Rest des Codes Ihrer Klasse,

- damit bestimmte Aspekte der globalen Klasse nachgestellt werden können. Hierzu kann beispielsweise der vollständige Datenbankzugriff auf eine separate lokale Klasse extrahiert werden, die in den Modultests durch eine Testattrappe ersetzt werden kann.

Lokale Klassen verhindern die Wiederverwendung, weil sie nicht an anderer Stelle verwendet werden können. Obwohl sie einfach zu extrahieren sind, ist es schwer, sie überhaupt zu finden. Dies führt zu unerwünschter Codeduplizierung. Orientierung, Navigation und Debugging in sehr langen Includes lokaler Klassen sind mühsam und lästig.
Da ABAP eine Include-Ebene sperrt, können an den verschiedenen Teilen des lokalen Includes nicht mehrere Personen gleichzeitig arbeiten (was möglich wäre, wenn es sich um separate globale Klassen handeln würde).

Überdenken Sie Ihre Verwendung von lokalen Klassen, wenn

- Ihr lokaler Include Dutzende von Klassen und Tausende von Codezeilen umfasst,
- Sie globale Klassen als „Pakete“ betrachten, die andere Klassen enthalten,
- Ihre globalen Klassen zu leeren Hülsen degenerieren,
- Sie doppelten Code über separate lokale Includes hinweg wiederholt finden,
- Ihre Entwickler beginnen, sich gegenseitig auszusperren, und zunehmend unfähig werden, parallel zu arbeiten,
- Ihre Rückstandschätzung an die Decke geht, weil Ihre verschiedenen Teams untereinander die lokalen Sub-Trees nicht verstehen 

#### FINAL, wenn keine Vererbung vorgesehen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Scope](#scope) > [Dieser Abschnitt](#final-if-not-designed-for-inheritance)

Machen Sie Klassen, die nicht explizit zur Vererbung vorgesehen sind, `FINAL`.

Beim Entwurf der Klassenkooperation sollte Ihre erste Wahl [Komposition, nicht die Vererbung](#prefer-composition-to-inheritance) sein.
Das Aktivieren der Vererbung ist nichts, was leichtfertig getan werden sollte, da Einiges zu bedenken ist, wie z.B. `PROTECTED` vs. `PRIVATE`, sowie das [Liskovsche Substitutionsprinzip](https://en.wikipedia.org/wiki/Liskov_substitution_principle). In ihr werden außerdem Entwurfs-Internal-Parts festgeschrieben. Wenn Sie diese Dinge nicht beim Entwurf Ihrer Klassen berücksichtigt haben, sollten Sie daher die unabsichtliche Vererbung verhindern, indem Sie Ihre Klasse `FINAL` machen.

Es gibt selbstverständlich einige _gute Einsatzmöglichkeiten für die Vererbung_, wie z.B. das Entwurfsmuster [composite](https://en.wikipedia.org/wiki/Composite_pattern).
Business Add-Ins können ebenfalls durch das Zulassen von Unterklassen nützlicher werden, da sie dem Kunde die Möglichkeit geben, den größten Teil des Ursprungscodes wiederzuverwenden. Beachten Sie jedoch, dass in all diesen Fällen die Vererbung von Anfang an mit Absicht eingebaut wird.

Unbereinigte Klassen, die keine [Schnittstellen implementieren](#public-instance-methods-should-be-part-of-an-interface), sollten Nicht-`FINAL` bleiben, damit sie von den Konsumenten in ihren Modultests nachgestellt werden können.

#### Mitglieder standardmäßig PRIVATE, nur im Bedarfsfall PROTECTED

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Scope](#scope) > [Dieser Abschnitt](#members-private-by-default-protected-only-if-needed)

Machen Sie Attribute, Methoden und andere Klassenmitglieder standardmäßig `PRIVATE`.

Machen Sie Attribute, Methoden und andere Klassenmitglieder nur `PROTECTED`, wenn Sie Unterklassen aktivieren möchten, die sie übersteuern.

Internal-Parts der Klassen sollten nur auf der Basis „so viel wie notwendig“ für andere Personen verfügbar gemacht werden. Dies bezieht sich nicht nur auf externe Aufrufer, sondern auch auf Unterklassen.
Werden die Informationen zu großzügig verfügbar gemacht, kann dies zu subtilen Fehlern durch unerwartete Redefinitionen führen und das Refactoring erschweren, da Außenstehende möglicherweise Mitglieder, die flexibel sein sollten, an Ort und Stelle fixieren.

#### Unveränderlichkeit anstelle des Getter erwägen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Scope](#scope) > [Dieser Abschnitt](#consider-using-immutable-instead-of-getter)

Ein unveränderliches Objekt ist ein Objekt, das sich nach seiner Konstruktion nicht mehr ändert. Für diese Objektart sollten Sie die Verwendung von öffentlichen Schreibschutzattributen anstelle der Getter-Methoden in Betracht ziehen.

```ABAP
CLASS /clean/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        a TYPE i
        b TYPE c
        c TYPE d.
    DATA a TYPE i READ-ONLY.
    DATA b TYPE c READ-ONLY.
    DATA c TYPE d READ-ONLY.
ENDCLASS.
```

anstelle von

```ABAP
CLASS /dirty/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS get_a ...
    METHODS get_b ...
    METHODS get_c ...
  PRIVATE SECTION.
    DATA a TYPE i.
    DATA b TYPE c.
    DATA c TYPE d.
ENDCLASS.
```

> **Achtung**: Für Objekte **mit veränderlichen Werten** keine öffentlichen Schreibschutzattribute verwenden. Andernfalls müssen diese Attribute immer aktuell gehalten werden, unabhängig davon, ob ihr Wert von anderem Code benötigt wird.

#### READ-ONLY sparsam verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Scope](#scope) > [Dieser Abschnitt](#use-read-only-sparingly)

Bei vielen modernen Programmiersprachen, allen voran Java, geht die Empfehlung dahin, Klassenmitglieder wo angebracht schreibgeschützt zu machen, um unabsichtliche Nebeneffekte zu vermeiden.

Auch wenn ABAP _tatsächlich_ den Zusatz `READ-ONLY` für Datendeklarationen anbietet, empfehlen wir dessen sparsame Verwendung.

Erstens ist der Zusatz nur im `PUBLIC SECTION` verfügbar, wodurch sich sein Geltungsbereich drastisch reduziert. Sie können ihn weder zu geschützten oder privaten Mitgliedern noch zu lokalen Variablen in einer Methode hinzufügen.

Zweitens funktioniert der Zusatz etwas anders als man von anderen Programmiersprachen voraussetzen könnte: Schreibgeschützte Daten können trotzdem von jeder Methode innerhalb der Klasse selbst, ihren Friends und ihren Unterklassen modifiziert werden. Dies widerspricht dem weiter verbreiteten Verhalten „einmal schreiben, niemals ändern“ in anderen Programmiersprachen. Dieser Unterschied kann zu bösen Überraschungen führen.

> Um Missverständnissen vorzubeugen: Variablen gegen versehentliche Modifikationen zu schützen, ist eine gute Praxis, die wir auch für ABAP empfehlen würden, wenn eine entsprechende Anweisung vorhanden wäre.

### Konstruktoren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Dieser Abschnitt](#constructors)

#### Besser NEW als CREATE OBJECT

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Konstruktoren](#constructors) > [Dieser Abschnitt](#prefer-new-to-create-object)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

Natürlich außer dort, wo Sie dynamische Typen benötigen

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### Bei globaler Klasse CREATE PRIVATE lassen Sie den CONSTRUCTOR öffentlich

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Konstruktoren](#constructors) > [Dieser Abschnitt](#if-your-global-class-is-create-private-leave-the-constructor-public)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

Zugegeben, dies ist ein Widerspruch. Gemäß Artikel [_Instanzkonstruktor_ der ABAP-Hilfe](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm)ist jedoch die Angabe des `CONSTRUCTOR` in der `PUBLIC SECTION` erforderlich, um eine korrekte Kompilierung und Syntaxprüfung zu gewährleisten.

Dies gilt nur für globale Klassen. Machen Sie in lokalen Klassen den Konstruktur privat, wie er sein sollte.

#### Besser mehrere statische Erstellungsmethoden als optionale Parameter

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Konstruktoren](#constructors) > [Dieser Abschnitt](#prefer-multiple-static-factory-methods-to-optional-parameters)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP unterstützt nicht das [Überladen](https://en.wikipedia.org/wiki/Function_overloading). Verwenden Sie Namensvariationen anstelle optionaler Parameter, um die gewünschte Semantik zu erzielen.

```ABAP
" anti-pattern
METHODS constructor
  IMPORTING
    data       TYPE any OPTIONAL
    name       TYPE any OPTIONAL
    object_ref TYPE REF TO object OPTIONAL
    data_ref   TYPE REF TO data OPTIONAL
  [...]
```

Die allgemeine Richtlinie
[_Besser Methoden aufteilen als OPTIONAL-Parameter hinzufügen_](#split-methods-instead-of-adding-optional-parameters) erläutert die Gründe, die dahinter stehen.

Erwägen Sie das Auflösen von komplexen Konstruktionen in eine Mehr-Schritte-Konstruktion mit dem [Entwurfsmuster Erbauer](https://en.wikipedia.org/wiki/Builder_pattern).

#### Aussagekräftige Namen bei mehreren Erstellungsmethoden verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Konstruktoren](#constructors) > [Dieser Abschnitt](#use-descriptive-names-for-multiple-creation-methods)

Geeignete Wörter für den Start der Methodenerstellung sind `new_`, `create_` und `construct_`. Sie werden intuitiv mit dem Aufbau von Objekten verknüpft und sind außerdem eine gute Ergänzung von Verbalphrasen wie `new_from_template`, `create_as_copy` oder `create_by_name`.

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

anstelle von etwas Bedeutungslosem wie

```ABAP
" anti-pattern
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### Singletons nur, wenn Multi-Instanzen keinen Sinn machen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Klassen](#classes) > [Konstruktoren](#constructors) > [Dieser Abschnitt](#make-singletons-only-where-multiple-instances-dont-make-sense)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

Wenden Sie das Singleton-Muster dort an, wo Ihr objektorientiertes Design vorgibt, dass die Existenz einer zweiten Instanz von Etwas keinen Sinn ergibt. Es eignet sich, wenn Sie sicherstellen möchten, dass jeder Konsument mit demselben Ding im selben Zustand und mit denselben Daten arbeitet.

Verwenden Sie das Singleton-Muster nicht aus Gewohnheit oder weil es Ihnen irgendeine Performance-Regel nahelegt. Es ist das am meisten überstrapazierte und falsch angewendete Muster, das nicht nur unerwarteten Nebenwirkungen verursacht, sondern auch das Testen unnötig erschwert.
Sind keine entwurfsbasierten Gründe für ein einheitliches Objekt vorhanden, überlassen Sie diese Entscheidung dem Konsumenten - er kann dasselbe immer noch außerhalb des Konstruktors erreichen, z.B. mittels Factory.

## Methoden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#methods)

Diese Regeln gelten für Methoden in Klassen und Funktionsbausteinen.

### Anrufe

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#calls)

#### Besser funktionale als prozedurale Aufrufe

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Aufrufe](#calls) > [Dieser Abschnitt](#prefer-functional-to-procedural-calls)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Wenn die dynamische Typisierung funktionale Aufrufe verbietet, greifen Sie auf prozedurale Aufrufe zurück.

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Viele der im Folgenden angeführten Detailregeln sind lediglich spezifischere Variationen dieser Empfehlung.

#### RECEIVING weglassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Aufrufe](#calls) > [Dieser Abschnitt](#omit-receiving)

```ABAP
DATA(sum) = aggregate_values( values ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### Optionales Schlüsselwort EXPORTING weglassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Aufrufe](#calls) > [Dieser Abschnitt](#omit-the-optional-keyword-exporting)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Parametername in einzelnen Parameteraufrufen weglassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Aufrufe](#calls) > [Dieser Abschnitt](#omit-the-parameter-name-in-single-parameter-calls)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates( list = list ).
```

Es gibt jedoch Fälle, in denen der Methodenname allein nicht klar genug ist, und die Wiederholung des Parameternamens die Verständlichkeit verbessern kann:

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### Eigenbezug me beim Aufruf einer Instanzmethode weglassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Aufrufe](#calls) > [Dieser Abschnitt](#omit-the-self-reference-me-when-calling-an-instance-method)

Da der Eigenbezug `me->` implizit vom System festgelegt wird, lassen Sie ihn beim Aufruf einer Instanzmethode weg.

```ABAP
DATA(sum) = aggregate_values( values ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
DATA(sum) = me->aggregate_values( values ).
```

### Methoden: Objektorientierung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#methods-object-orientation)

#### Besser Instanzmethode als statische Methode

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Methoden: Objektorientierung](#methods-object-orientation) > [Dieser Abschnitt](#prefer-instance-to-static-methods)

Methoden sollten standardmäßig Instanzmitglieder sein. Instanzmethoden reflektieren das „Objektartige“ der Klasse auf bessere Weise und können einfacher in Modultests nachgestellt werden.

```ABAP
METHODS publish.
```

Methoden sollten nur in Ausnahmefällen statisch sein, wie z.B. statische Erstellungsmethoden.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### Öffentliche Instanzmethoden sollten Teil einer Schnittstelle sein

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Methoden: Objektorientierung](#methods-object-orientation) > [Dieser Abschnitt](#public-instance-methods-should-be-part-of-an-interface)

Öffentliche Instanzmethoden sollten immer Teil einer Schnittstelle sein. Hierdurch werden Abhängigkeiten entkoppelt, und das Nachstellen der Methoden in Modultests wird einfacher.

```ABAP
METHOD /clean/blog_post~publish.
```

Im Rahmen des Clean-Object-Ansatzes macht es nicht viel Sinn, eine Methode ohne eine Schnittstelle öffentlich zu machen - mit wenigen Ausnahmen, wie z.B. den Enumerationsklassen, die nie eine alternative Implementierung haben und nie in Testfällen nachgestellt werden.

> [Interfaces vs. Abstract Classes](sub-sections/InterfacesVsAbstractClasses.md)
führt aus, warum dies auch für Klassen gilt, die vererbte Methoden überschreiben.

### Parameteranzahl

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#parameter-number)

#### So wenig IMPORTING-Parameter wie möglich, im Bestfall weniger als drei

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameteranzahl](#parameter-number) > [Dieser Abschnitt](#aim-for-few-importing-parameters-at-best-less-than-three)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

wäre sehr viel klarer als

```ABAP
" anti-pattern
FUNCTION seo_class_copy
  IMPORTING
    clskey                 TYPE seoclskey
    new_clskey             TYPE seoclskey
    access_permission      TYPE seox_boolean DEFAULT seox_true
    VALUE(save)            TYPE seox_boolean DEFAULT seox_true
    VALUE(suppress_corr)   TYPE seox_boolean DEFAULT seox_false
    VALUE(suppress_dialog) TYPE seox_boolean DEFAULT seox_false
    VALUE(authority_check) TYPE seox_boolean DEFAULT seox_true
    lifecycle_manager      TYPE REF TO if_adt_lifecycle_manager OPTIONAL
    lock_handle            TYPE REF TO if_adt_lock_handle OPTIONAL
    VALUE(suppress_commit) TYPE seox_boolean DEFAULT seox_false
  EXPORTING
    ...
```

Zu viele Eingabeparameter lassen die Komplexität einer Methode explodieren, weil sie eine überproportionale Anzahl von Kombinationen verarbeiten muss. Eine große Anzahl von Parametern ist ein Hinweis darauf, dass die Methode möglicherweise mehr als eine Sache tut.

Sie können die Anzahl der Parameter reduzieren, indem Sie sie in sinnvollen Gruppen mit Strukturen und Objekten zusammenfassen.

#### Besser Methoden aufteilen als OPTIONAL-Parameter hinzufügen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameteranzahl](#parameter-number) > [Dieser Abschnitt](#split-methods-instead-of-adding-optional-parameters)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

zum Erzielen der gewünschten Semantik, da ABAP kein [Überladen](https://en.wikipedia.org/wiki/Function_overloading)unterstützt.

```ABAP
" anti-pattern
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

Optionale Parameter verwirren Aufrufer:

- Welche werden wirklich benötigt?
- Welche Kombinationen sind gültig?
- Welche schließen einander aus?

Mehrere Methoden mit bestimmen Parametern für den Use-Case verhindern diese Verwirrung, indem sie klar vorschreiben, welche Parameterkombinationen gültig sind und erwartet werden.

#### PREFERRED PARAMETER sparsam verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameteranzahl](#parameter-number) > [Dieser Abschnitt](#use-preferred-parameter-sparingly)

Der Zusatz `PREFERRED PARAMETER` macht es schwer zu erkennen, welcher Parameter tatsächlich bereitgestellt wird, und erschwert somit die Verständlichkeit des Codes.
Indem die Anzahl der Parameter, insbesondere der optionalen Parameter, reduziert wird, sinkt automatisch der Bedarf nach dem Zusatz `PREFERRED PARAMETER`.

#### RETURN, EXPORT oder CHANGE - nur eins davon

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameteranzahl](#parameter-number) > [Dieser Abschnitt](#return-export-or-change-exactly-one-parameter)

Eine gute Methode tut _eine Sache_, und dies sollte sich darin widerspiegeln, dass die Methode auch genau eine Sache zurückgibt. Wenn die Ausgabeparameter Ihrer Methode _keine_ logische Einheit bilden, tut Ihre Methode mehr als eine Sache, und Sie sollten sie aufteilen.

Es gibt Fälle, in denen die Ausgabe eine logische Einheit ist, die aus mehreren Dingen besteht. Diese werden am einfachsten durch die Rückgabe einer Struktur oder eines Objekts dargestellt:

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

anstelle von

```ABAP
" anti-pattern
METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  EXPORTING
    result            TYPE result_type
    failed_keys       TYPE /bobf/t_frw_key
    messages          TYPE /bobf/t_frw_message.
```

Insbesondere im Vergleich zu mehreren EXPORTING-Parametern erlaubt dies die Verwendung funktionaler Aufrufe, erspart das Nachdenken über `IS SUPPLIED` und verhindert, dass der Abruf von vitalen `ERROR_OCCURRED`-Informationen vergessen wird.

Ziehen Sie anstelle von mehreren optionalen Ausgabeparametern die Aufteilung der Methode nach sinnvollen Aufrufmustern in Betracht:

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE result_type.

METHODS check_and_report
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

### Parametertypen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#parameter-types)

#### Besser RETURNING als EXPORTING

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parametertypen](#parameter-types) > [Dieser Abschnitt](#prefer-returning-to-exporting)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
METHODS square
  IMPORTING
    number TYPE i
  EXPORTING
    result TYPE i.

square(
  EXPORTING
    number = 42
  IMPORTING
    result = DATA(result) ).
```

`RETURNING` verkürzt nicht nur den Aufruf, sondern ermöglicht auch die Methodenverkettung und verhindert Fehler vom Typ [identische Ein- und Ausgabe](#take-care-if-input-and-output-could-be-the-same).

#### RETURNING von großen Tabellen ist in der Regel problemlos

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parametertypen](#parameter-types) > [Dieser Abschnitt](#returning-large-tables-is-usually-okay)

Trotz anderslautender Aussagen in der ABAP-Dokumentation und den Performance-Leitfäden stoßen wir selten auf Fälle, in denen die Übergabe einer großen oder stark verschachtelten Tabelle in einem VALUE-Parameter _tatsächlich zu Performanceproblemen führt_. Wir empfehlen daher, die aktuelle Verwendung

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

Nur wenn ein tatsächlicher Beweis (= schlechte Performance-Messung) für Ihren individuellen Fall vorliegt, greifen Sie auf den umständlicheren prozeduralen Stil zurück

```ABAP
" anti-pattern
METHODS get_large_table
  EXPORTING
    result TYPE /dirty/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

get_large_table( IMPORTING result = DATA(my_table) ).
```

> Dieser Abschnitt widerspricht den ABAP-Programmierrichtlinien und Code-Inspector-Prüfungen, die Beide vorschlagen, dass große Tabellen exportiert werden sollten, um Performance-Defizite zu vermeiden. Die Reproduktion jeglicher Performance- und Speicherdefizite ist uns nicht gelungen, und wir erhielten Mitteilung über die System-Kernel-Optimierung, die im Allgemeinen die RETURNING-Performance verbessert.

#### RETURNING oder EXPORTING oder CHANGING verwenden, jedoch keine Kombination

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parametertypen](#parameter-types) > [Dieser Abschnitt](#use-either-returning-or-exporting-or-changing-but-not-a-combination)

```ABAP
METHODS copy_class
  IMPORTING
    old_name      TYPE seoclsname
    new name      TYPE secolsname
  RETURNING
    VALUE(result) TYPE copy_result
  RAISING
    /clean/class_copy_failure.
```

anstelle einer verwirrenden Mischung wie

```ABAP
" anti-pattern
METHODS copy_class
  ...
  RETURNING
    VALUE(result)      TYPE vseoclass
  EXPORTING
    error_occurred     TYPE abap_bool
  CHANGING
    correction_request TYPE trkorr
    package            TYPE devclass.
```

Verschiedene Sorten von Ausgabeparametern sind ein Hinweis darauf, dass die Methode mehr als eine Sache tut. Dies verwirrt den Leser und macht den Aufruf der Methode unnötig kompliziert.

Eine akzeptable Ausnahme zu dieser Regel können Builder sein, die ihre Eingabe konsumieren, während sie ihre Ausgabe aufbauen:

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

Selbst diese können jedoch klarer gemacht werden, indem die Eingabe objektifiziert wird:

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### CHANGING sparsam verwenden, wo geeignet

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parametertypen](#parameter-types) > [Dieser Abschnitt](#use-changing-sparingly-where-suited)

`CHANGING` sollte für Fälle reserviert werden, in denen eine vorhandene lokale Variable, die bereits befüllt ist, nur an einigen Stellen aktualisiert wird:

```ABAP
METHODS update_references
  IMPORTING
    new_reference TYPE /bobf/conf_key
  CHANGING
    bo_nodes      TYPE root_nodes.

METHOD update_references.
  LOOP AT bo_nodes REFERENCE INTO DATA(bo_node).
    bo_node->reference = new_reference.
  ENDLOOP.
ENDMETHOD.
```

Zwingen Sie Ihre Aufrufer nicht, unnötige lokale Variablen einzuführen, nur um Ihren `CHANGING`-Parameter bereitzustellen. Verwenden Sie keine `CHANGING`-Parameter zum initialen Befüllen einer zuvor leeren Variable.

#### Aufgeteilte Methode statt boolescher Eingabeparameter

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parametertypen](#parameter-types) > [Dieser Abschnitt](#split-method-instead-of-boolean-input-parameter)

Boolesche Eingabeparameter sind häufig ein Hinweis darauf, dass eine Methode _zwei_ Dinge anstelle von einem tut.

```ABAP
" anti-pattern
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

Methodenaufrufe mit einem einzelnen - und daher unbenannten - booleschen Parameter machen außerdem die Bedeutung des Parameters in vielen Fällen undeutlich.

```ABAP
" anti-pattern
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

Die Aufteilung der Methode kann den Methodencode vereinfachen und die verschiedenen Zwecke besser beschreiben

```ABAP
update_without_saving( ).
update_and_save( ).
```

Die allgemeine Auffassung legt nahe, dass Setter für boolesche Variablen in Ordnung sind:

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> Mehr erfahren Sie in
[1](http://www.beyondcode.org/articles/booleanVariables.html)
[2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/)
[3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### Parameternamen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#parameter-names)

#### RETURNING-Parameters evtl. RESULT nennen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameternamen](#parameter-names) > [Dieser Abschnitt](#consider-calling-the-returning-parameter-result)

Gute Methodennamen sind in der Regel so gut, dass der `RETURNING`-Parameter keinen eigenen Namen braucht. Der Name wäre nicht viel mehr als das Nachgeplapper des Methodennamens oder die Wiederholung von etwas Offensichtlichem.

Die Wiederholung eines Mitgliednamens kann sogar zu Konflikten führen, die durch das Hinzufügen eines überflüssigen `me->` gelöst werden müssen.

```ABAP
" anti-pattern
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

Nennen Sie in diesen Fällen den Parameter einfach `RESULT` oder etwas wie `RV_RESULT`, wenn Sie die Ungarische Notation bevorzugen.

Benennen Sie den `RETURNING`-Parameter, wenn es _nicht_ offensichtlich ist, wofür er steht, z.B. in Methoden, die `me` für die Methodenverkettung zurückgeben, oder in Methoden, die etwas erzeugen, die erzeugte Entität jedoch nicht zurückgeben, sondern beispielsweise nur den Schlüssel.

### Parameterinitialisierung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#parameter-initialization)

#### EXPORTING-Referenzparameter löschen oder überschreiben

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameterinitialisierung](#parameter-initialization) > [Dieser Abschnitt](#clear-or-overwrite-exporting-reference-parameters)

Referenzparameter beziehen sich auf vorhandene Speicherbereiche, die vorab befüllt werden können. Löschen oder überscheiben Sie diese, um zuverlässige Daten bereitzustellen:

```ABAP
METHODS square
  EXPORTING
    result TYPE i.

" clear
METHOD square.
  CLEAR result.
  " ...
ENDMETHOD.

" overwrite
METHOD square.
  result = cl_abap_math=>square( 2 ).
ENDMETHOD.
```

> Code Inspector und CheckMan verweisen auf `EXPORTING`-Variablen, die nie geschrieben werden. Nutzen Sie diese statischen Prüfungen, um diese andernfalls ziemlich undurchsichtige Fehlerquelle zu vermeiden.

##### Vorsicht bei identischer Ein- und Ausgabe

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameterinitialisierung](#parameter-initialization) > [Dieser Abschnitt](#take-care-if-input-and-output-could-be-the-same)

Im Allgemeinen ist es eine gute Idee, den Parameter als Erstes in der Methode nach den Typ- und Datendeklarationen zu löschen. Dies macht die Anweisung einfach auffindbar und verhindert, dass der noch enthaltene Wert versehentlich von späteren Anweisungen verwendet wird.

Einige Parameterkonfigurationen könnten dieselbe Variable jedoch als Ein- und Ausgabe verwenden. In diesem Fall würde ein verfrühtes `CLEAR` den Eingabewert löschen, bevor er verwendet werden kann, und zu falschen Ergebnissen führen.

```ABAP
" anti-pattern
DATA value TYPE i.

square_dirty(
  EXPORTING
    number = value
  IMPORTING
    result = value ).

METHOD square_dirty.
  CLEAR result.
  result = number * number.
ENDMETHOD.
```

Erwägen Sie die Neugestaltung dieser Methoden, indem Sie `EXPORTING` durch `RETURNING` ersetzen. Überlegen Sie auch die Überschreibung des `EXPORTING`-Parameters in einer Berechnungsanweisung mit einem Einzelergebnis. Wenn keine der beiden Möglichkeiten passt, verwenden Sie ein `CLEAR` an später Stelle.

#### VALUE-Parameter nicht löschen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Parameterinitialisierung](#parameter-initialization) > [Dieser Abschnitt](#dont-clear-value-parameters)

Parameter, die mit `VALUE` arbeiten, werden als neue, separate Speicherbereiche übergeben, die per Definition leer sind. Löschen Sie diese nicht noch einmal:

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

`RETURNING`-Parameter sind immer `VALUE`-Parameter und müssen daher nie gelöscht werden:

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### Methodenrumpf

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#method-body)

#### Mache eine Sache zur Zeit, und mache sie gut

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Methodenrumpf](#method-body) > [Dieser Abschnitt](#do-one-thing-do-it-well-do-it-only)

Eine Methode sollte eine Sache tun, und nur eine Sache. Diese sollte sie auf bestmögliche Weise tun.

Eine Methode tut wahrscheinlich eine Sache, wenn

- sie [wenige Eingabeparameter](#aim-for-few-importing-parameters-at-best-less-than-three) hat,
- die [keine booleschen Parameter](#split-method-instead-of-boolean-input-parameter) beinhalten
- sie [genau einen Ausgabeparameter](#return-export-or-change-exactly-one-parameter) hat
- sie [klein](#keep-methods-small) ist
- sie [eine Abstraktionsebene tiefer ](#descend-one-level-of-abstraction) ist
- Sie keine weiteren sinnvollen Methoden extrahieren können
- Sie ihre Anweisungen nicht sinnvoll in Abschnitte gruppieren können

#### Glücklicher Pfad oder Fehlerbehebung, nicht Beides

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Methodenrumpf](#method-body) > [Dieser Abschnitt](#focus-on-the-happy-path-or-error-handling-but-not-both)

Als Spezialfall der Regel [_Mache eine Sache zur Zeit, und mache sie gut_](#do-one-thing-do-it-well-do-it-only) sollte eine Methode entweder dem glücklichen Pfad folgen, für den sie erzeugt wurde, oder dem Umweg der Fehlerbehandlung, sofern der glückliche Pfad nicht möglich ist. Beides gleichzeitig ist keine gangbare Alternative.

```ABAP
" anti-pattern
METHOD append_xs.
  IF input > 0.
    DATA(remainder) = input.
    WHILE remainder > 0.
      result = result && `X`.
      remainder = remainder - 1.
    ENDWHILE.
  ELSEIF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSE.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.
```

Kann zerlegt werden in

```ABAP
METHOD append_xs.
  validate( input ).
  DATA(remainder) = input.
  WHILE remainder > 0.
    result = result && `X`.
    remainder = remainder - 1.
  ENDWHILE.
ENDMETHOD.

METHOD validate.
  IF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSEIF input < 0.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.
```

oder, um den Validierungsteil hervorzuheben

```ABAP
METHOD append_xs.
  IF input > 0.
    result = append_xs_without_check( input ).
  ELSEIF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSE.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.

METHOD append_xs_without_check.
  DATA(remainder) = input.
  WHILE remainder > 0.
    result = result && `X`.
    remainder = remainder - 1.
  ENDWHILE.
ENDMETHOD.
```

#### Eine Abstraktionsebene tiefer steigen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Methodenrumpf](#method-body) > [Dieser Abschnitt](#descend-one-level-of-abstraction)

Anweisungen in einer Methode sollten sich eine Abstraktionsebene unter der Methode selbst befinden. Dementsprechend sollten sie sich alle auf derselben Abstraktionsebene befinden.

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

anstelle einer verwirrenden Mischung von Konzepten auf niedriger Ebene (`trim`, `to_upper`, ...) und höherer Ebene (`publish`, ...) wie

```ABAP
" anti-pattern
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

Eine zuverlässige Methode, herauszufinden, welches die richtige Abstraktionsebene ist, sieht folgendermaßen aus: Lassen Sie den Autor der Methode in wenigen, kurzen Worten erläutern, was die Methode tut, jedoch ohne dabei den Code anzusehen. Die Punkte, die der Autor aufführt, sind die Untermethoden, die von der Methode aufgerufen werden sollen, oder die Anweisungen, die sie ausführen soll.

#### Methoden klein halten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Methodenrumpf](#method-body) > [Dieser Abschnitt](#keep-methods-small)

Methoden sollten weniger als 20 Anweisungen haben, idealerweise 3 bis 5.

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

Die folgende `DATA`-Deklaration allein reicht aus, um zu erkennen, dass die umgebende Methode sehr viel mehr als eine Sache tut:

```ABAP
" anti-pattern
DATA:
  class           TYPE vseoclass,
  attributes      TYPE seoo_attributes_r,
  methods         TYPE seoo_methods_r,
  events          TYPE seoo_events_r,
  types           TYPE seoo_types_r,
  aliases         TYPE seoo_aliases_r,
  implementings   TYPE seor_implementings_r,
  inheritance     TYPE vseoextend,
  friendships     TYPE seof_friendships_r,
  typepusages     TYPE seot_typepusages_r,
  clsdeferrds     TYPE seot_clsdeferrds_r,
  intdeferrds     TYPE seot_intdeferrds_r,
  attribute       TYPE vseoattrib,
  method          TYPE vseomethod,
  event           TYPE vseoevent,
  type            TYPE vseotype,
  alias           TYPE seoaliases,
  implementing    TYPE vseoimplem,
  friendship      TYPE seofriends,
  typepusage      TYPE vseotypep,
  clsdeferrd      TYPE vseocdefer,
  intdeferrd      TYPE vseoidefer,
  new_clskey_save TYPE seoclskey.
```

Natürlich gibt es Fälle, in denen es keinen Sinn ergibt, eine umfangreiche Methode weiter aufzuteilen. Dies ist völlig in Ordnung, sofern die Methode [auf eine Sache fokussiert bleibt](#do-one-thing-do-it-well-do-it-only):

```ABAP
METHOD decide_what_to_do.
  CASE temperature.
    WHEN burning.
      result = air_conditioning.
    WHEN hot.
      result = ice_cream.
    WHEN moderate.
      result = chill.
    WHEN cold.
      result = skiing.
    WHEN freezing.
      result = hot_cocoa.
  ENDCASE.
ENDMETHOD.
```

Es macht jedoch trotzdem Sinn, zu prüfen, ob der ausführliche Code ein geeigneteres Muster verbirgt:

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> Das Zerlegen von Methoden in Mikrogröße kann sich negativ auf die Performance auswirken, da sich hierdurch die Anzahl der Methodenaufrufe erhöht. Der [Abschnitt _Performance beachten_](#mind-the-performance) gibt Empfehlungen, wie Sie die richtige Balance zwischen Clean Code und Performance finden können.

### Kontrollfluss

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Dieser Abschnitt](#control-flow)

#### Früh scheitern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Kontrollfluss](#control-flow) > [Dieser Abschnitt](#fail-fast)

Sie sollten so früh wie möglich validieren und scheitern.

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

Spätere Validierungen sind schwieriger erkennbar und verständlich, und sie haben bis zu diesem Punkt möglicherweise schon Ressourcen verschwendet.

```ABAP
" anti-pattern
METHOD do_something.
  DATA(massive_object) = build_expensive_object_from( input ).
  IF massive_object IS NOT BOUND. " happens if input is initial
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

#### CHECK vs. RETURN

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Kontrollfluss](#control-flow) > [Dieser Abschnitt](#check-vs-return)

Es gibt keinen Konsens darüber, ob Sie `CHECK` oder `RETURN` zum Beenden einer Methode verwenden sollten, wenn die Eingabe nicht den Erwartungen entspricht.

Während `CHECK` definitiv die kürzere Syntax bereitstellt,

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

gibt der Name der Anweisung nicht preis, was passiert, wenn die Bedingung fehlschlägt. Daher ist die Langform wahrscheinlich verständlicher:

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD:
```

Sie können die Frage vollständig umgehen, indem Sie die Validierung umkehren und einen Kontrollfluss mit nur einer Rückgabe übernehmen.

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD:
```

Überlegen Sie in jedem Fall, ob das Zurückgeben von „Nichts“ wirklich das geeignete Verhalten ist. Methoden sollten ein sinnvolles Ergebnis bereitstellen, d.h. entweder einen befüllten Rückgabeparameter oder eine Ausnahme. Keine Rückgabe entspricht in vielen Fällen der Rückgabe von `null`, was vermieden werden sollte.

> Der [Abschnitt _Prozeduren verlassen_ in den ABAP-Programmierrichtlinien](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)empfiehlt die Verwendung von `CHECK` in diesem Fall. Diskussionen in der Community legen nahe, dass die Anweisung unklar ist und dazu führt, dass das Programmverhalten nicht verständlich ist.

#### CHECK an anderer Stelle vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Methoden](#methods) > [Kontrollfluss](#control-flow) > [Dieser Abschnitt](#avoid-check-in-other-positions)

Verwenden Sie `CHECK` nicht außerhalb des Initialisierungsabschnitts einer Methode. Das Verhalten der Anweisung variiert abhängig von ihrer Position und kann zu unklaren, unerwarteten Ergebnissen führen.

So beendet [`CHECK` in einer `LOOP` beispielsweise die aktuelle Iteration und fährt mit der nächsten fort](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm), anstatt, wie möglicherweise irrtümlich erwartet, die Methode bzw. die Loop zu beenden.

> Basierend auf [Abschnitt _Prozeduren verlassen_ in den ABAP-Programmierrichtlinien](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm).
Beachten Sie, dass dies der [Schlüsselwortreferenz für `CHECK` in Loops](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm)widerspricht.

## Fehlerbehandlung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#error-handling)

### Meldungen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Dieser Abschnitt](#messages)

#### Nachrichten leicht auffindbar machen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Nachrichten](#messages) > [Dieser Abschnitt](#make-messages-easy-to-find)

Um Meldungen über eine Verwendungssuche der Transaktion SE91 leicht auffindbar zu machen, verwenden Sie das folgende Muster:

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

Sofern die Variable `message` nicht benötigt wird, fügen Sie das Pragma `##NEEDED` hinzu:

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

Vermeiden Sie Folgendes:

```ABAP
" anti-pattern
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

Dies ist ein Anti-Pattern, weil:
- es unerreichbaren Code enthält
- es eine Bedingung testet, die für Gleichheit niemals wahr sein kann

### Rückgabecodes

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Dieser Abschnitt](#return-codes)

#### Ausnahmen statt Rückgabecodes

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Rückgabecodes](#return-codes) > [Dieser Abschnitt](#prefer-exceptions-to-return-codes)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

anstelle von

```ABAP
" anti-pattern
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

Ausnahmen haben gegenüber Rückgabecodes viele Vorteile:

- Ausnahmen halten Ihre Methodensignaturen sauber: Sie können das Ergebnis der Methode als `RETURNING`-Parameter zurückgeben und trotzdem nebenher Ausnahmen absetzen. Rückgabecodes verunreinigen dagegen Ihre Signaturen mit Zusatzparametern für die Fehlerbehandlung.

- Der Aufrufer muss nicht sofort auf sie reagieren. Er kann einfach den glücklichen Pfad seines Codes herunterschreiben. Die Ausnahmebehandlung `CATCH` kann sich ganz am Ende dieser Methode oder völlig außerhalb befinden.

- Die Attribute und Methoden von Ausnahmen können Details zu dem Fehler liefern. Rückgabecodes erfordern, dass Sie sich selbst eine Alternativlösung ausdenken, wie z.B. die Rückgabe eines Protokolls. 

- Die Umgebung erinnert den Aufrufer mit Syntaxfehlern daran, Ausnahmen zu bearbeiten. Rückgabecodes können versehentlich ignoriert werden, ohne dass es jemand bemerkt.

#### Alle Fehler abfangen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Rückgabecodes](#return-codes) > [Dieser Abschnitt](#dont-let-failures-slip-through)

Wenn Sie wirklich Rückgabecodes verwenden müssen, z.B. weil Sie Funktionen und älteren Code aufrufen, die außerhalb Ihrer Kontrolle sind, stellen Sie sicher, dass Ihnen keine Fehler entgehen.

```ABAP
DATA:
  current_date TYPE string,
  response     TYPE bapiret2.

CALL FUNCTION 'BAPI_GET_CURRENT_DATE'
  IMPORTING
    current_date = current_date
  CHANGING
    response     = response.

IF response-type = 'E'.
  RAISE EXCEPTION NEW /clean/some_error( );
ENDIF.
```

### Ausnahmen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Dieser Abschnitt](#exceptions)

#### Ausnahmen sind für Fehler gedacht, nicht für den Normalfall

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen](#exceptions) > [Dieser Abschnitt](#exceptions-are-for-errors-not-for-regular-cases)

```ABAP
" anti-pattern
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

Für reguläre, gültige Fälle sollten reguläre Ergebnisparameter verwendet werden.

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

Ausnahmen sollten den Fällen vorbehalten werden, die unerwartet sind und Fehlersituationen widerspiegeln.

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

Der missbräuchliche Einsatz von Ausnahmen verleitet den Leser dazu, anzunehmen, dass etwas falsch ist, obwohl in Wirklichkeit alles in Ordnung ist. Ausnahmen sind außerdem viel langsamer als der reguläre Code, weil sie konstruiert werden müssen und häufig eine Menge Kontextinformationen sammeln.

#### Klassenbasierte Ausnahmen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen](#exceptions) > [Dieser Abschnitt](#use-class-based-exceptions)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

Die veralteten, nicht klassenbasierten Ausnahmen haben dieselben Funktionen wie Rückgabecodes und sollten nicht mehr verwendet werden.

```ABAP
" anti-pattern
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### Ausnahme absetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Dieser Abschnitt](#throwing)

#### Eigene übergeordnete Klassen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#use-own-super-classes)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```

Erwägen Sie die Erzeugung von abstrakten übergeordneten Klassen für jeden Ausnahmetyp Ihrer Anwendung, anstatt die Foundation-Klassen direkt mit Unterklassen auszustatten. Dies gestattet Ihnen das `CATCH` von _allen_ Ihren Ausnahmen.
Ermöglicht Ihnen das Hinzufügen von allgemeinen Funktionen zu allen Ausnahmen, wie z.B. spezielle Textbehandlung.
`ABSTRACT` verhindert die unabsichtliche direkte Verwendung dieser nicht beschriebenen Fehler.

#### Einen Ausnahmetyp zur Zeit absetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#throw-one-type-of-exception)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

In der überwiegenden Mehrheit der Fälle hat das Absetzen mehrerer Ausnahmetypen keinen Sinn.
Der Aufrufer ist gewöhnlich weder interessiert noch in der Lage, die Fehlersituationen auseinanderzuhalten.
Er wird sie daher in der Regel alle auf dieselbe Weise beheben, wodurch der Sinn der ursprünglichen Unterscheidung dieser Fehler zunichte gemacht wird. 

```ABAP
" anti-pattern
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

Eine bessere Lösung zur Erkennung der verschiedenen Fehlersituationen besteht darin, einen einzigen Ausnahmetyp zu verwenden, jedoch Unterklassen hinzuzufügen, die eine Reaktion auf individuelle Fehlersituationen gestatten, jedoch nicht erfordern. Siehe hierzu Abschnitt [Übersichtlichere Fehlersituationen mit untergeordneten Klassen](#use-sub-classes-to-enable-callers-to-distinguish-error-situations).

#### Übersichtlichere Fehlersituationen mit untergeordneten Klassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#use-sub-classes-to-enable-callers-to-distinguish-error-situations)

```ABAP
CLASS cx_bad_generation_variable DEFINITION INHERITING FROM cx_generation_error.
CLASS cx_bad_code_composer_template DEFINITION INHERITING FROM cx_generation_error.

TRY.
    generator->generate( ).
  CATCH cx_bad_generation_variable.
    log_failure( ).
  CATCH cx_bad_code_composer_template INTO DATA(bad_template_exception).
    show_error_to_user( bad_template_exception ).
  CATCH cx_generation_error INTO DATA(other_exception).
    RAISE EXCEPTION NEW cx_application_error( previous =  other_exception ).
ENDTRY.
```

Bestehen zahlreiche unterschiedliche Fehlersituationen, verwenden Sie stattdessen Fehlercodes:

```ABAP
CLASS cx_generation_error DEFINITION ...
  PUBLIC SECTION.
    TYPES error_code_type TYPE i.
    CONSTANTS:
      BEGIN OF error_code_enum,
        bad_generation_variable    TYPE error_code_type VALUE 1,
        bad_code_composer_template TYPE error_code_type VALUE 2,
        ...
      END OF error_code_enum.
    DATA error_code TYPE error_code_type.

TRY.
    generator->generate( ).
  CATCH cx_generation_error INTO DATA(exception).
    CASE exception->error_code.
      WHEN cx_generation_error=>error_code_enum-bad_generation_variable.
      WHEN cx_generation_error=>error_code_enum-bad_code_composer_variable.
      ...
    ENDCASE.
ENDTRY.
```

#### CX_STATIC_CHECK für überschaubare Ausnahmen absetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#throw-cx_static_check-for-manageable-exceptions)

Wenn eine Ausnahme erwartet und vom Empfänger auf angemessene Art behandelt werden kann, setzen Sie eine geprüfte Ausnahmevererbung über `CX_STATIC_CHECK` ab: fehlerhafte Validierung der Benutzereingabe, fehlende Ressource, zu der Fallbacks existieren usw.

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

Dieser Ausnahmetyp _muss_ in Methodensignaturen angegeben _und_ abgefangen oder weitergeleitet werden, um Syntaxfehler zu vermeiden.
Er ist daher für den Konsumenten offensichtlich und stellt sicher, dass dieser nicht von einer unerwarteten Ausnahme überrascht wird, und angemessen auf die Fehlersituation reagiert.

> Diese Empfehlung entspricht den [ABAP-Programmierrichtlinien](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm), widerspricht jedoch [Robert C. Martins _Clean Code_], in dem die Bevorzugung ungeprüfter Ausnahmen empfohlen wird. Abschnitt [Exceptions](sub-sections/Exceptions.md) erklärt, warum.

#### CX_NO_CHECK für gewöhnlich nicht behebbare Situationen absetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#throw-cx_no_check-for-usually-unrecoverable-situations)

Wenn eine Ausnahme so schwer ist, dass sich der Empfänger wahrscheinlich nicht davon erholt, verwenden Sie `CX_NO_CHECK`: Fehler beim Lesen einer obligatorischen Quelle, Fehler beim Auflösen der angeforderten Abhängigkeit usw.

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` _kann nicht_ in Methodensignaturen deklariert werden und ist somit für den Konsumenten eine böse Überraschung. Im Falle von nicht behebbaren Situationen ist dies in Ordnung, weil der Konsument ohnehin nicht hilfreich darauf reagieren kann.

Es _kann_ jedoch Fälle geben, in denen der Konsument tatsächlich diese Art von Fehler erkennen und darauf reagieren möchte. Beispiel: Ein Dependency Manager setzt `CX_NO_CHECK` ab, wenn er nicht in der Lage ist, eine Implementierung für eine angeforderte Schnittstelle bereitzustellen, weil der reguläre Anwendungscode nicht fortfahren kann.
Es ist jedoch möglicherweise ein Testreport vorhanden, der versucht, alles Mögliche zu instanziieren, nur um zu sehen, ob es funktioniert, und der den Fehler einfach nur als roter Eintrag in einer Liste meldet.
Dieser Service sollte in der Lage sein, die Ausnahme abzufangen und zu ignorieren, anstatt zu einem Dump gezwungen zu werden.

#### CX_DYNAMIC_CHECK für vermeidbare Ausnahmen absetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#consider-cx_dynamic_check-for-avoidable-exceptions)

Use-Cases für `CX_DYNAMIC_CHECK` sind selten, und im Allgemeinen empfehlen wir, auf die anderen Ausnahmetypen zurückzugreifen. Sie können diesen Ausnahmetyp jedoch als Ersatz für `CX_STATIC_CHECK` erwägen, wenn der Aufrufe volle, bewusste Kontrolle darüber hat, ob eine Ausnahme auftreten kann.

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

Denken Sie beispielsweise an die Methode `get_db_length_decs` von Klasse `cl_abap_math`, die Ihnen die Anzahl der Ziffern und Nachkommastellen einer dezimalen Gleitpunktzahl mitteilt. Diese Methode setzt die dynamische Ausnahme `cx_parameter_invalid_type` ab, wenn der Eingabeparameter keine dezimale Gleitpunktzahl widerspiegelt. Diese Methode wird gewöhnlich für eine vollständige, statische Variable aufgerufen, so dass der Entwickler weiß, ob diese Ausnahme jemals auftreten kann.
In diesem Fall würde die dynamische Ausnahme dem Aufrufer gestatten, die überflüssige `CATCH`-Klausel wegzulassen.

#### Dump für schwerwiegende, nicht behebbare Situationen absetzen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#dump-for-totally-unrecoverable-situations)

Wenn eine Situation so schwerwiegend ist, dass Sie ganz sicher davon ausgehen, dass sich der Empfänger nicht davon erholen wird, oder eine Situation ganz klar auf einen Programmierfehler hinweist, erzeugen Sie einen Dump, anstatt eine Ausnahme abzusetzen: Fehler beim Speicherabruf, fehlender Index-Lesevorgang in einer Tabelle, die befüllt werden muss usw.

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

Dieses Verhalten führt dazu, dass kein Konsument anschließend irgendwelche sinnvollen Schritte ausführen kann. Auf einen Dump sollte daher nur zurückgegriffen werden, wenn Sie sich sicher sind.

#### Besser RAISE EXCEPTION NEW als RAISE EXCEPTION TYPE

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen absetzen](#throwing) > [Dieser Abschnitt](#prefer-raise-exception-new-to-raise-exception-type)

Hinweis: ab NW 7.52 verfügbar.

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

ist im Allgemeinen kürzer als das überflüssigerweise längere 

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

Wenn Sie jedoch massiven Gebrauch vom Zusatz `MESSAGE` machen, sollten Sie bei der `TYPE`-Variante bleiben:

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception
  MESSAGE e136(messages).
```

### Ausnahmen abfangen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Dieser Abschnitt](#catching)

#### Externe Ausnahmen umschließen, um das Eindringen in Ihren Code zu verhindern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Fehlerbehandlung](#error-handling) > [Ausnahmen abfangen](#catching) > [Dieser Abschnitt](#wrap-foreign-exceptions-instead-of-letting-them-invade-your-code)

```ABAP
METHODS generate RAISING cx_generation_failure.

METHOD generate.
  TRY.
      generator->generate( ).
    CATCH cx_amdp_generation_failure INTO DATA(exception).
      RAISE EXCEPTION NEW cx_generation_failure( previous = exception ).
  ENDTRY.
ENDMETHOD.
```

Das [Gesetz der Demeter](https://en.wikipedia.org/wiki/Law_of_Demeter) empfiehlt die Entkopplung der Dinge. Die Weiterleitung von Ausnahmen aus anderen Komponenten verstößt gegen dieses Prinzip.
Machen Sie sich unabhängig vom Fremdcode, indem Sie diese Ausnahmen abfangen und in einem eigenen Ausnahmetyp umschließen.

```ABAP
" anti-pattern
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## Kommentare

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#comments)

### In Code ausdrücken, nicht in Kommentaren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#express-yourself-in-code-not-in-comments)

```ABAP
METHOD correct_day_to_last_in_month.
  WHILE is_invalid( date ).
    reduce_day_by_one( CHANGING date = date ).
  ENDWHILE.
ENDMETHOD.

METHOD is_invalid.
  DATA zero_if_invalid TYPE i.
  zero_if_invalid = date.
  result = xsdbool( zero_if_invalid = 0 ).
ENDMETHOD.

METHOD reduce_day_by_one.
  date+6(2) = date+6(2) - 1.
ENDMETHOD.
```

anstelle von

```ABAP
" anti-pattern
" correct e.g. 29.02. in non-leap years as well as result of a date calculation would be
" something like e.g. the 31.06. that example has to be corrected to 30.06.
METHOD fix_day_overflow.
  DO 3 TIMES.
    " 31 - 28 = 3 => this correction is required not more than 3 times
    lv_dummy = cv_date.
    " lv_dummy is 0 if the date value is a not existing date - ABAP specific implementation
    IF ( lv_dummy EQ 0 ).
      cv_date+6(2) = cv_date+6(2) - 1. " subtract 1 day from the given date
    ELSE.
      " date exists => no correction required
      EXIT.
    ENDIF.
  ENDDO.
ENDMETHOD.
```

Clean Code verbietet Ihnen _nicht_ das Kommentieren Ihres Codes, sondern ermutigt Sie dazu, _bessere_ Mittel zu nutzen, und nur dann auf Kommentare zurückzugreifen, wenn Sie anders nicht zum gewünschten Ergebnis kommen.

> Bei diesem Beispiel wurde der Performance-Aspekt infrage gestellt, mit der Begründung, dass eine so kleinteilige Aufteilung der Methoden die Performance zu stark beeinträchtigt. Beispielmessungen zeigen, dass der Code nach dem Refactoring 2,13 mal langsamer ist als die ursprüngliche unbereinigte Variante.
Die bereinigte Variante braucht 9,6 Mikrosekunden, um die Eingabe `31-02-2018` zu verarbeiten, die unbereinigte Variante nur 4,5 Mikrosekunden. Dies mag ein Problem sein, wenn die Methode sehr häufig in einer High-Performance-Anwendung ausgeführt wird, in einer normalen Benutzereingabe-Validierung sollte sie jedoch akzeptabel sein. Weitere Informationen zu Clean Code und Performanceproblemen finden Sie im Abschnitt [Performance beachten](#mind-the-performance).

### Kommentare sind keine Ausrede für schlechte Namenswahl

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#comments-are-no-excuse-for-bad-names)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

Verwenden Sie bessere Namen, anstatt zu erläutern, was Sie wirklich meinen, oder warum Sie einen ungeeigneten Namen gewählt haben.

```ABAP
" anti-pattern
" checks whether the table input contains entries
DATA(result) = check_table( input ).
```

### Methoden statt Kommentaren zur Code-Segmentierung verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#use-methods-instead-of-comments-to-segment-your-code)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

Dies macht nicht nur die Absicht, Struktur und Abhängigkeiten des Codes sehr viel klarer, sondern vermeidet auch Folgefehler, wenn temporäre Variable zwischen den Abschnitten nicht ordnungsgemäß zurückgesetzt werden.

```ABAP
" anti-pattern
" -----------------
" Build statement
" -----------------
DATA statement TYPE string.
statement = |SELECT * FROM d_document_roots|.

" -----------------
" Execute statement
" -----------------
DATA(result_set) = adbc->execute_sql_query( statement ).
result_set->next_package( IMPORTING data = data ).
```

### Mit Kommentaren das Warum, nicht das Was erläutern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#write-comments-to-explain-the-why-not-the-what)

```ABAP
" can't fail, existence of >= 1 row asserted above
DATA(first_line) = table[ 1 ].
```

Niemand benötigt eine Wiederholung des Codes in natürlicher Sprache.

```ABAP
" anti-pattern
" select alert root from database by key
SELECT * FROM d_alert_root WHERE key = key.
```

### Design gehört in das Design-Dokument, nicht in den Code

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#design-goes-into-the-design-documents-not-the-code)

```ABAP
" anti-pattern
" This class serves a double purpose. First, it does one thing. Then, it does another thing.
" It does so by executing a lot of code that is distributed over the local helper classes.
" To understand what's going on, let us at first ponder the nature of the universe as such.
" Have a look at this and that to get the details.
```

Ehrlich - niemand liest das. Wenn ein Fachbuch erforderlich ist, um Ihren Code zu verstehen, kann dies ein Hinweis darauf sein, dass Ihr Code ernsthafte Design-Probleme hat, die Sie auf andere Weise lösen sollten.
Wenn Ihr Code _wirklich_ eine Erläuterung über eine einzelne Kommentarzeile hinaus erfordert, was durchaus der Fall sein kann, schlagen wir eine Verlinkung mit dem Design-Dokument vor.

### Kommentare mit ", nicht mit * markieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#comment-with--not-with-)

Zitieren Sie Kommentare eingerückt, gemeinsam mit den Anweisungen, die sie kommentieren

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

Kommentare mit Asterisk tendieren zum Einrücken an seltsame Positionen

```ABAP
" anti-pattern
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### Kommentare gehören vor die Anweisung, auf die sie sich beziehen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#put-comments-before-the-statement-they-relate-to)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

Klarer als

```ABAP
" anti-pattern
output = calculate_result( input ).
" delegate pattern
```

Und weniger invasiv als

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### Code löschen, nicht kommentieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#delete-code-instead-of-commenting-it)

```ABAP
" anti-pattern
* output = calculate_result( input ).
```

Wenn Sie etwas wie das finden, löschen Sie es.
Der Code wird offensichtlich nicht benötigt, weil Ihre Anwendung funktioniert und alle Tests grün sind. Gelöschter Code kann später aus der Versionshistorie heraus reproduziert werden.
Wenn Sie Code permanent konservieren möchten, kopieren Sie ihn in eine Datei oder ein `$TMP`- oder `HOME`-Objekt.

### FIXME, TODO und XXX verwenden, und Ihre ID hinzufügen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#use-fixme-todo-and-xxx-and-add-your-id)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME` verweist auf Fehler, die zu klein oder noch zu sehr in der Mache sind für interne Meldungen.
- `TODO`s sind Stellen, an denen Sie etwas in der näheren (!) Zukunft  vervollständigen möchten.
- `XXX` markiert Code, der funktioniert, aber verbesserungswürdig ist.

Wenn Sie einen solchen Kommentar erfassen, fügen Sie Ihren Nicknamen, Ihre Initialen oder Ihren Benutzernamen hinzu, damit Ihre Co-Entwickler Sie kontaktieren und fragen können, wenn der Kommentar unklar ist.

### Kein Kommentar zu Methodensignatur und Ende

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#dont-add-method-signature-and-end-of-comments)

Methodensignatur-Kommentare nützen niemandem etwas.

```ABAP
" anti-pattern
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CALIBRATION_KPIS=>CALCULATE_KPI
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRATEGY_ID                 TYPE        STRATEGY_ID
* | [--->] THRESHOLD                   TYPE        STRATEGY_THRESHOLD
* | [--->] DETECTION_OBJECT_SCORE      TYPE        T_HIT_RESULT
* | [<---] KPI                         TYPE        T_SIMULATED_KPI
* +--------------------------------------------------------------------------------------</SIGNATURE>
```

Vor Jahrzehnten, als Sie beim Prüfen des Codes die Methodensignatur nicht sehen konnten, oder mit Ausdrucken arbeiteten, die Dutzende von Seiten umfassten, mögen diese Kommentare sinnvoll gewesen sein. Alle modernen ABAP IDEs (SE24, SE80, ADT) zeigen die Methodensignatur jedoch offensichtlich, sodass diese Kommentare überflüssig geworden sind.

> Im formularbasierten Editor von SE24/SE80 verwenden Sie die Drucktaste _Signatur_.
In den ABAP Development Tools markieren Sie den Methodennamen und verwenden Sie F2 oder fügen Sie die Sicht _ABAP Element Info_ zu Ihrer Perspektive hinzu.

Entsprechend sind auch Endekommentare überflüssig. Diese Kommentare waren vor Jahrzehnten möglicherweise hilfreich, als Programme, Funktionen und die darin verschachtelten IFs Hunderte von Codezeilen umfassten. Der moderne Kodierungsstil erzeugt jedoch Methoden, bei deren Kürze mühelos ersichtlich ist, zu welcher Eröffnungsanweisung ein `ENDIF` oder `ENDMETHOD` gehört:

```ABAP
" anti-pattern
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### Meldungstexte nicht in Kommentaren wiederholen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#dont-duplicate-message-texts-as-comments)

```ABAP
" anti-pattern
" alert category not filled
MESSAGE e003 INTO dummy.
```

Meldungen ändern sich unabhängig von Ihrem Code, und niemand wird daran denken, den Kommentar zu aktualisieren, so dass er veraltet und sogar sehr schnell irreführend wird, ohne dass es jemand bemerkt.

Die modernen IDEs machen es Ihnen einfach, sich den Text einer Meldung anzeigen zu lassen. In den ABAP Development Tools markieren Sie beispielsweise die Message-ID und drücken Shift+F2.

Wenn Sie es expliziter wünschen, können Sie die Meldung auch in eine eigene Methode extrahieren.

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### ABAP Doc nur für öffentliche APIs

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#abap-doc-only-for-public-apis)

Schreiben Sie ein ABAP Doc zur Dokumentation von öffentlichen APIs, d.h. APIs, die für Entwickler in anderen Teams oder Anwendungen bestimmt sind. Schreiben Sie kein ABAP Doc für die interne Angelegenheiten.

ABAP Doc leidet unter denselben Schwächen wie alle Kommentare - schnelle Veraltung und anschließende Missverständlichkeit. Folglich sollte ABAP Doc nur dort verwendet werden, wo es Sinn macht, und nicht zu jedem Zweck und überall.

> Mehr erfahren Sie in _Kapitel 4: Gute Kommentare: Javadocs in öffentlichen APIs_ und _Kapitel 4: Schlechte Kommentare: Javadocs in nicht-öffentlichem Code_ in [Robert C. Martins _Clean Code_].

### Besser Pragmas als Pseudokommentare

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Kommentare](#comments) > [Dieser Abschnitt](#prefer-pragmas-to-pseudo-comments)

Ziehen Sie Pragmas den Pseudokommentaren vor, um irrelevante Warnungen und Fehler zu unterdrücken, die vom ATC identifiziert werden. Pseudokommentare sind größtenteils obsolet geworden und wurden durch Pragmas ersetzt.

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" anti-pattern
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

Nutzen Sie das Programm `ABAP_SLIN_PRAGMAS` oder die Tabelle `SLIN_DESC` zum Auffinden der Zuordnung zwischen obsoleten Pseudokommentaren und den Pragmas, durch die sie ersetzt wurden.

## Formatierungen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#formatting)

Die folgenden Vorschläge sind [optimiert zum Lesen, nicht zum Schreiben](#optimize-for-reading-not-for-writing).
Da ABAP Pretty Printer diese nicht abdeckt, verursachen einige davon manuellen Zusatzaufwand beim Umformatieren von Anweisungen, wenn sich die Namenslängen usw. ändern. Wenn Sie dies vermeiden möchten, überlegen Sie, auf Regeln wie [Zuordnung zum selben Objekt verdeutlichen](#align-assignments-to-the-same-object-but-not-to-different-ones) zu verzichten.

### Konsistent sein

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#be-consistent)

Formatieren Sie den gesamten Code eines Projekts auf dieselbe Weise. Lassen Sie alle Teammitglieder denselben Formatierungsstil nutzen.

Wenn Sie Fremdcode bearbeiten, halten Sie sich an den Formatierungsstil dieses Projekts, anstatt auf Ihrem persönlichen Stil zu beharren.

Wenn Sie Ihre Formatierungsregeln im Zeitverlauf ändern, verwenden Sie die [Best Practices für das Refactoring](#how-to-refactor-legacy-code), um Ihren Code nach und nach zu aktualisieren.

### Zum Lesen optimieren, nicht zum Schreiben

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#optimize-for-reading-not-for-writing)

Entwickler verbringen die meiste Zeit mit dem _Lesen_ von Code.
Das eigentliche _Schreiben_ des Codes nimmt einen wesentlich kleineren Teil der Arbeitszeit in Anspruch.

Folglich sollten Sie Ihre Code-Formatierung zum Lesen und Debugging optimieren, und nicht zum Schreiben.

Verwenden Sie also lieber

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

als Hacks, wie z.B.

```ABAP
" anti-pattern
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### Pretty Printer vor der Aktivierung verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#use-the-pretty-printer-before-activating)

Verwenden Sie Pretty Printer - Shift+F1 in SE80, SE24 und ADT - vor dem Aktivieren eines Objekts.

Wenn Sie eine größere, unformatierte Legacy-Codebasis ändern, empfiehlt sich die Anwendung von Pretty Printer nur auf ausgewählte Zeilen, um umfangreiche Änderungslisten und Transportabhängigkeiten zu vermeiden. Wenn Sie das vollständige Entwicklungsobjekt mit Pretty Printer bearbeiten möchten, können Sie dies per separaten Transportauftrag oder Hinweis tun.

> Mehr erfahren Sie in _Kapitel 5: Formatierung: Team-Regeln_ von [Robert C. Martins _Clean Code_].

### Ihre Pretty-Printer-Teameinstellungen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#use-your-pretty-printer-team-settings)

Verwenden Sie immer Ihre Teameinstellungen. Diese geben Sie an unter _Menü_ > _Hilfsmittel_ > _Einstellungen ..._ > _ABAP Editor_ > _Pretty Printer_.

Legen Sie _Einrücken_ und _Groß-/Kleinkonvertierung durchführen_ > _Schlüsselwort groß_ fest, wie in Ihrem Team vereinbart.

> [Upper vs. Lower Case](sub-sections/UpperVsLowerCase.md) erläutert, warum wir keine klare Anweisung für Groß-/Kleinschreibung von Schlüsselwörtern geben.
Mehr erfahren Sie in _Kapitel 5: Formatierung: Team-Regeln_ von [Robert C. Martins _Clean Code_].

### Maximal eine Anweisung pro Zeile

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#no-more-than-one-statement-per-line)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

Auch wenn es gelegentlich so scheint, als wäre dieser Code lesbar:

```ABAP
" anti-pattern
DATA do_this TYPE i. do_this = input + 3.
```

### Vernünftige Zeilenlänge einhalten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#stick-to-a-reasonable-line-length)

Halten Sie sich an eine maximale Zeilenlänge von 120 Zeichen.

Für das menschliche Auge ist das Lesen von Texten angenehmer, wenn die Zeilen nicht zu breit sind - fragen Sie hierzu einen UI Designer oder Spezialisten der Augenbewegungsforschung Ihrer Wahl. Sie werden den schmaleren Code auch beim Debugging oder Vergleichen von zwei nebeneinanderstehenden Quellen zu schätzen wissen.

Die Begrenzung auf 80 oder sogar 72 Zeichen, die noch von den Anforderungen alter Terminalgeräte stammt, ist etwas zu restriktiv. Während 100 Zeichen oft empfohlen werden und eine machbare Wahl sind, scheinen 120 Zeichen für ABAP etwas besser zu funktionieren, möglicherweise aufgrund der allgemeinen Ausführlichkeit der Sprache.

> Zur Erinnerung: In den ADT können Sie den Seitenrand auf 120 Zeichen festlegen. Dieser wird dann in der Code-Ansicht als vertikale Linie dargestellt. Den Seitenrand konfigurieren Sie unter _Menu_ > _Window_ > _Preferences_ > _General_ > _Editors_ > _Text Editors_.

### Ihren Code kondensieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#condense-your-code)

```ABAP
DATA(result) = calculate( items ).
```

anstatt unnötige Leerzeichen hinzuzufügen

```ABAP
" anti-pattern
DATA(result)        =      calculate(    items =   items )   .
```

### Nur eine Leerzeile zum Trennen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#add-a-single-blank-line-to-separate-things-but-not-more)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

zum Hervorheben, dass die beiden Anweisungen unterschiedliche Dinge tun. Es gibt jedoch keinen Grund für

```ABAP
" anti-pattern
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

Das Bedürfnis nach trennenden Leerzeilen kann ein Hinweis darauf sein, dass Ihre Methode [nicht eine Sache tut](#do-one-thing-do-it-well-do-it-only).

### Keine exzessiven Leerzeilen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#dont-obsess-with-separating-blank-lines)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

Es gibt keinen Grund für die schlechte Gewohnheit, Ihren Code mit Leerzeilen auseinanderzureißen.

```ABAP
" anti-pattern
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

Leerzeilen machen tatsächlich nur Sinn, wenn Sie Anweisungen haben, die mehrere Zeilen umspannen

```ABAP
METHOD do_something.

  do_this( ).

  then_that(
    EXPORTING
      variable = 'A'
    IMPORTING
      result   = result ).

ENDMETHOD.
```

### Zuordnung zum selben Objekt verdeutlichen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#align-assignments-to-the-same-object-but-not-to-different-ones)

Um hervorzuheben, dass diese Dinge irgendwie zusammengehören

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

oder sogar besser

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

Wenn die Dinge nichts miteinander zu tun haben, belassen Sie jedoch diese Form:

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> Mehr erfahren Sie in _Kapitel 5: Formatierung: Horizontale Ausrichtung_ von [Robert C. Martins _Clean Code_].

### Klammern am Zeilenende schließen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#close-brackets-at-line-end)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### Einzelne Parameteraufrufe auf einer Zeile belassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#keep-single-parameter-calls-on-one-line)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

anstelle des unnötig längeren

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### Parameter hinter dem Aufruf angeben

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#keep-parameters-behind-the-call)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Wenn dies die Zeilen zu lang macht, können Sie die Parameter in die nächste Zeile umbrechen:

```ABAP
DATA(sum) = add_two_numbers(
                   value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                   value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### Bei Zeilenumbruch Parameter unter dem Aufruf einrücken

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#if-you-break-indent-parameters-under-the-call)

```ABAP
DATA(sum) = add_two_numbers(
                   value_1 = 5
                   value_2 = 6 ).
```

Wenn Sie die Parameter anders anordnen, ist schwer zu erkennen, wozu sie gehören:

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

Dies ist jedoch das beste Muster, wenn Sie vermeiden möchten, dass die Formatierung durch eine Namenslängenänderung zerstört wird.

### Zeilenumbruch bei mehreren Parametern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#line-break-multiple-parameters)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Zugegeben, es ist Platzverschwendung. Andernfalls ist jedoch schwer zu erkennen, wo ein Parameter endet und der nächste beginnt:

```ABAP
" anti-pattern
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### Parameter anordnen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#align-parameters)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

Flatterränder machen es schwer zu erkennen, wo der Parameter endet und sein Wert beginnt:

```ABAP
" anti-pattern
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> Dies ist andererseits das beste Muster, wenn Sie vermeiden möchten, dass die Formatierung durch eine Namenslängenänderung zerstört wird.

### Aufruf auf eine neue Zeile umbrechen, wenn die Zeile zu lang wird

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#break-the-call-to-a-new-line-if-the-line-gets-too-long)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### Einrücken und Tabulator verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#indent-and-snap-to-tab)

Rücken Sie Parameterschlüsselwörter um 2 Stellen und Parameter um 4 Stellen ein:

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

Wenn Sie keine Schlüsselwörter haben, rücken Sie die Parameter um 4 Stellen ein.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Verwenden Sie die Tabulatortaste zum Einrücken. Es ist in Ordnung, wenn hierdurch eine Leerstelle mehr als nötig hinzugefügt wird. (Dies geschieht, wenn der `DATA(sum) =`-Teil links eine ungerade Zeichenanzahl hat.)

### Inline-Deklarationen wie Methodenaufrufe einrücken

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#indent-in-line-declarations-like-method-calls)

Rücken Sie Inline-Deklarationen mit VALUE oder NEW wie Methodenaufrufe ein:

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### Type-Klauseln nicht ausrichten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Formatierung](#formatting) > [Dieser Abschnitt](#dont-align-type-clauses)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

Eine Variable und ihr Typ gehören zusammen und sollten daher nahe zusammenstehen. Durch die Ausrichtung der `TYPE`-Klauseln wird die Aufmerksamkeit von dieser Zusammengehörigkeit abgelenkt und suggeriert, dass die Variablen eine vertikale Gruppe bilden, und ihre Typen eine andere. Die Ausrichtung verursacht außerdem unnötigen Bearbeitungsaufwand, da bei einer Änderung des längsten Variablennamens alle Einrückungen angepasst werden müssen.

```ABAP
" anti-pattern
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

## Test

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Dieser Abschnitt](#testing)

### Grundlagen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#principles)

#### Testbaren Code schreiben

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Grundlagen](#principles) > [Dieser Abschnitt](#write-testable-code)

Schreiben Sie den gesamten Code so, dass Sie ihn automatisch testen können.

Wenn dies ein Refactoring Ihres Codes erfordert, tun Sie es. Tun Sie dies zuerst, bevor Sie mit dem Hinzufügen von weiteren Funktionen beginnen.

Wenn Sie Legacy-Code ergänzen, der zu schlecht strukturiert ist, um ihn zu testen, führen Sie ein Refactoring zumindest in dem Umfang aus, dass Sie Ihre Ergänzungen testen können.

#### Nachstellen erlaubt

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Grundlagen](#principles) > [Dieser Abschnitt](#enable-others-to-mock-you)

Wenn Sie Code schreiben, der von Anderen konsumiert werden soll, ermöglichen Sie diesen, Modultests für ihren eigenen Code zu schreiben. Dies ist z.B. möglich durch Hinzufügen von Schnittstellen an nach außen gerichteten Stellen, durch die Bereitstellung von hilfreichen Testattrappen, die Integrationstests ermöglichen, oder durch die Anwendung der Abhängigkeitsumkehr, die eine Ersetzung der produktiven Konfiguration durch eine Testkonfiguration ermöglicht.

#### Regeln für die Lesbarkeit

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Grundlagen](#principles) > [Dieser Abschnitt](#readability-rules)

Machen Sie Ihren Testcode noch besser lesbar als Ihren produktiven Code. Sie können schlechten produktiven Code mit guten Tests in Angriff nehmen, aber wenn die Tests nicht funktionieren, sind Sie verloren.

Machen Sie Ihren Testcode so einfach und simpel, dass Sie ihn auch noch in einem Jahren verstehen.

Halten Sie sich an Standards und Muster, damit sich Ihre Kollegen schnell in den Code einlesen können.

#### Keine Kopien oder Testreports

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Grundlagen](#principles) > [Dieser Abschnitt](#dont-make-copies-or-write-test-reports)

Fangen Sie nicht damit an, ein Backlog abzuarbeiten, indem Sie eine `$TMP`-Kopie eines Entwicklungsobjekts anfertigen und mit diesem herumspielen. Andere werden diese Objekte nicht bemerken und daher den Status Ihrer Arbeit nicht kennen. Sie werden wahrscheinlich eine Menge Zeit alleine mit dem Anfertigen der Arbeitskopie vergeuden. Hinterher werden Sie möglicherweise vergessen, die Kopie zu löschen, und damit Ihr System und die Abhängigkeiten spammen. (Wenn Sie das nicht glauben, gehen Sie jetzt in Ihr Entwicklungssystem und prüfen Sie `$TMP`.)

Es ist außerdem nicht ratsam, einen Testreport zu schreiben, der etwas auf bestimmte Weise aufruft, und diesen Report während Ihrer Arbeit wiederholt auszuführen, um zu prüfen, ob alles noch funktioniert. Es ist nicht sehr professionell, einen Testreport manuell zu wiederholen und auf Sicht zu prüfen, ob alles noch gut ist. Nehmen Sie den nächsten Schritt in Angriff und automatisieren Sie diesen Report in einem Modultest mit einer automatischen Rückmeldung darüber, ob der Code noch in Ordnung ist. Erstens ersparen Sie sich den Aufwand, die Modultests hinterher schreiben zu müssen. Zweitens ersparen Sie sich eine Menge Zeit, die Sie andernfalls für die manuellen Wiederholungen aufwenden würden, sowie eine Menge Langeweile.

#### Nur Public-Parts testen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Grundlagen](#principles) > [Dieser Abschnitt](#test-publics-not-private-internals)

Public-Parts von Klassen, insbesondere die Schnittstellen, die sie implementieren, sind ziemlich stabil und ändern sich mit großer Wahrscheinlichkeit nicht. Lassen Sie Ihre Modultests nur die Public-Parts validieren, um sie robust zu machen und den Aufwand bei einem Refactoring der Klasse zu minimieren. Geschützte und private Internal-Parts können sich im Gegensatz dazu sehr schnell durch das Refactoring ändern, so dass jedes Refactoring Ihre Tests unnötigerweise unterbrechen würde.

Ein dringendes Bedürfnis, private oder geschützte Methoden zu testen, kann ein frühes Warnzeichen für mehrere Arten von Design-Fehlern sein.
Fragen Sie sich selbst:

- Haben Sie versehentlich ein Konzept in Ihrer Klasse begraben, das in seiner eigenen Klasse herauskommen möchte, mit seinem eigenen, dedizierten Testpaket?

- Haben Sie versäumt, die Domänenlogik vom Glue Code zu trennen? So ist z.B. die Implementierung der Domänenlogik direkt in der Klasse, die im BOPF als Aktion, Festlegung oder Validierung integriert ist, oder die von SAP Gateway als `*_DPC_EXT`-Daten-Provider generiert wurde, möglicherweise nicht die beste Idee.

- Sind Ihre Schnittstellen zu kompliziert und fordern zu viele Daten an, die irrelevant sind oder nicht einfach nachgestellt werden können?

#### Zum Thema Quelltextabdeckung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Grundlagen](#principles) > [Dieser Abschnitt](#dont-obsess-about-coverage)

Die Quelltextabdeckung soll Ihnen dabei helfen, versehentlich ungeprüften Code zu finden, und nicht, irgendeine KPI zu erfüllen:

Erfinden Sie keine Tests mit oder ohne Dummy-Assertionen, nur um die Abdeckung zu erzielen.
Lassen Sie Dinge besser ungeprüft, um transparent zu machen, dass ihr sicheres Refactoring nicht mögich ist. Sie können eine Abdeckung von < 100 % und trotzdem perfekte Tests haben.
Es gibt Fälle - wie z.B. IFs im Konstruktor zum Einfügen von Testattrappen -, die das Erreichen von 100 % unpraktikabel machen.
Gute Tests decken in der Regel dieselbe Anweisung mehrfach ab, für verschiedene Verzweigungen und Bedingungen.
Sie haben in der Tat eine imaginäre Abdeckung von > 100 %.

### Testklassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#test-classes)

#### Lokale Testklassen nach ihrem Zweck benennen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testklassen](#test-classes) > [Dieser Abschnitt](#call-local-test-classes-by-their-purpose)

```ABAP
CLASS ltc_unit_tests DEFINITION FOR TESTING ... .
CLASS ltc_integration_tests DEFINITION FOR TESTING ... .
CLASS ltc_unit_tests_with_mocks DEFINITION FOR TESTING ... .
```

Gute Namen enthüllen die Stufe der Tests und die Gemeinsamkeit in ihrem Setup.

```ABAP
" anti-patterns
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " We know that's the class under test - why repeat it?
CLASS ltc_test DEFINITION FOR TESTING ....                      " Of course it's a test, what else should it be?
```

#### Tests in lokale Klassen integrieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testklassen](#test-classes) > [Dieser Abschnitt](#put-tests-in-local-classes)

Integrieren Sie die Modultests in das lokale Test-Include der getesteten Klasse. Hierdurch wird sichergestellt, dass diese Tests beim Refactoring der Klasse wiedergefunden werden und alle verbundenen Tests mit einem einzigen Tastendruck ausgeführt werden können, wie in [Testklassen ausführen](#how-to-execute-test-classes) beschrieben.

Integrieren Sie Komponenten-, Integrations- und Systemtests in das lokale Test-Include einer separaten globalen Klasse. 
Sie beziehen sich nicht direkt auf eine einzelne getestete Klasse, daher sollten sie nicht willkürlich in eine der beteiligten Klassen gestellt werden, sondern in eine getrennte Klasse.
Kennzeichnen Sie diese globale Testklasse als `FOR TESTING` und `ABSTRACT`, um zu vermeiden, dass sie versehentlich im Produktionscode referenziert wird.
Wenn Tests in andere Klassen gestellt werden, besteht die Gefahr, dass sie übersehen und beim Refactoring der beteiligten Klassen vergessen werden.

Daher ist es von Vorteil, *Testbeziehungen* zu nutzen, um zu dokumentieren, welche Objekte vom Test abgedeckt werden. Mit dem Beispiel unten könnte die Testklasse `hiring_test` in Klasse `recruting` oder `candidate` mit dem Shortcut `Shift-Crtl-F12` (Windows) bzw. `Cmd-Shift-F12` (macOS) ausgeführt werden.

```abap
"! @testing recruting
"! @testing candidate
class hiring_test defintion
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### Hilfsmethoden in Hilfeklassen integrieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testklassen](#test-classes) > [Dieser Abschnitt](#put-help-methods-in-help-classes)

Integrieren Sie Hilfsmethoden, die von mehreren Testklassen verwendet werden, in eine Hilfsklasse. Machen Sie die Hilfsmethoden über Vererbung (ist eine Beziehung) oder Delegation (hat eine Beziehung) verfügbar.

```abap
" inheritance example

CLASS lth_unit_tests DEFINITION ABSTRACT FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PROTECTED SECTION.
    CLASS-METHODS assert_activity_entity
      IMPORTING
        actual_activity_entity TYPE REF TO zcl_activity_entity
        expected_activity_entity TYPE REF TO zcl_activity_entity.
    ...
ENDCLASS.

CLASS lth_unit_tests IMPLEMENTATION.

  METHOD assert_activity_entity.
    ...
  ENDMETHOD.

ENDCLASS.

CLASS ltc_unit_tests DEFINITION INHERITING FROM lth_unit_tests FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  ...
ENDCLASS.
```

#### Testklassen ausführen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testklassen](#test-classes) > [Dieser Abschnitt](#how-to-execute-test-classes)

Drücken Sie in den ABAP Development Tools Strg+Shift+F10, um alle Tests in einer Klasse auszuführen.
Drücken Sie Strg+Shift+F11, um eine Abdeckungsmessungen einzubeziehen.
Drücken Sie Strg+Shift+F12, um Tests auch in anderen Klassen auszuführen, die als Testbeziehungen gepflegt sind.

> Verwenden Sie auf macOS `Cmd` anstelle von `Ctrl`.

### Getesteter Code

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#code-under-test)

#### Sinnvolle Code-Namen oder Standardname CUT

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Getesteter Code](#code-under-test) > [Dieser Abschnitt](#name-the-code-under-test-meaningfully-or-default-to-cut)

Geben Sie der Variable, die den getesteten Code darstellt, einen sinnvollen Namen:

```ABAP
DATA blog_post TYPE REF TO ...
```

Wiederholen Sie nicht einfach den Klassennamen mit seinen wenig aussagefähigen Namensräumen und Präfixen:

```ABAP
" anti-pattern
DATA clean_fra_blog_post TYPE REF TO ...
```

Wenn Sie unterschiedliche Test-Setups haben, kann es hilfreich sein, den variierenden Objektzustand zu beschreiben:

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

Wenn Sie Probleme haben, einen sinnvollen Namen zu finden, verwenden Sie standardmäßig `cut`.
Die Abkürzung steht für „Code unter Test“.

```ABAP
DATA cut TYPE REF TO ...
```

Insbesondere in unsauberen und verwirrenden Tests kann das Aufrufen der Variable `cut` dem Leser vorübergehend helfen, zu erkennen, was tatsächlich getestet wird. Langfristig ist die Bereinigung der Tests jedoch der richtige Weg.

#### Schnittstellen testen, nicht Klassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Getesteter Code](#code-under-test) > [Dieser Abschnitt](#test-interfaces-not-classes)

Als praktische Konsequenz von [_Nur Public-Parts testen_](#test-publics-not-private-internals) geben Sie für den Typ Ihres getesteten Codes eine _Schnittstelle_ an,

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

anstelle einer _Klasse_

```ABAP
" anti-pattern
DATA code_under_test TYPE REF TO some_class.
```

#### Aufruf an getesteten Code in seine eigene Methode extrahieren

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Getesteter Code](#code-under-test) > [Dieser Abschnitt](#extract-the-call-to-the-code-under-test-to-its-own-method)

Wenn die zu testende Methode eine Menge Parameter und aufbereitete Daten erfordert, kann es helfen, den Aufruf an die Methode in eine eigene Hilfsmethode zu extrahieren, die die unkritischen Parameter vorbelegt:

```ABAP
METHODS map_xml_to_itab
  IMPORTING
    xml_string TYPE string
    config     TYPE /clean/xml2itab_config DEFAULT default_config
    format     TYPE /clean/xml2itab_format DEFAULT default_format.

METHOD map_xml_to_itab.
  result = cut->map_xml_to_itab( xml_string = xml_string
                                 config     = config
                                 format     = format ).
ENDMETHOD.

DATA(itab) = map_xml_to_itab( '<xml></xml>' ).
```

Durch den direkten Aufruf der ursprünglichen Methode kann Ihr Test mit einer Menge bedeutungsloser Details überflutet werden:

```ABAP
" anti-pattern
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### Injection

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#injection)

#### Abhängigkeitsumkehr zum Einbringen von Testattrappen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#use-dependency-inversion-to-inject-test-doubles)

Abhängigkeitsumkehr bedeutet, dass Sie alle Abhängigkeiten an den Konstruktor übergeben:

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

Verwenden Sie nicht die Setter Injection. Sie gestattet die Nutzung des produktiven Codes auf nicht vorgesehene Weise:

```ABAP
" anti-pattern
METHODS set_customizing_reader
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD do_something.
  object->set_customizing_reader( a ).
  object->set_customizing_reader( b ). " would you expect that somebody does this?
ENDMETHOD.
```

Verwenden Sie nicht die FRIENDS Injection.
Sie initialisiert produktive Abhängigkeiten, bevor diese ersetzt werden, mit wahrscheinlich unerwarteten Folgen. 
Sie funktioniert nicht mehr, sobald Sie die Internal-Parts umbenennen.
Außerdem verhindert sie Initialisierungen im Konstruktor.

```ABAP
" anti-pattern
METHOD setup.
  cut = NEW fra_my_class( ). " <- builds a productive customizing_reader first - what will it break with that?
  cut->customizing_reader ?= cl_abap_testdouble=>create( 'if_fra_cust_obj_model_reader' ).
ENDMETHOD.

METHOD constructor.
  customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
  customizing_reader->fill_buffer( ). " <- won't be called on your test double, so no chance to test this
ENDMETHOD.
```

#### ABAP-Testattrappe verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#consider-to-use-the-tool-abap-test-double)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

Kürzer und besser verständlich als angepasste Testattrappen:

```ABAP
" anti-pattern
CLASS /dirty/default_custom_reader DEFINITION FOR TESTING CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dirty/customizing_reader.
    DATA customizing TYPE /dirty/customizing_table.
ENDCLASS.

CLASS /dirty/default_custom_reader IMPLEMENTATION.
  METHOD /dirty/customizing_reader~read.
    result = customizing.
  ENDMETHOD.
ENDCLASS.

METHOD test_something.
  DATA(customizing_reader) = NEW /dirty/customizing_reader( ).
  customizing_reader->customizing = sub_claim_customizing.
ENDMETHOD.
```

#### Von Test-Tools unterstützen lassen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#exploit-the-test-tools)

Im Allgemeinen können Sie bei einem sauberen Programmierstil viele Aufgaben mit den standardmäßigen ABAP-Modultests und -Testattrappen erledigen.
Es stehen weitere Tools zur Verfügung, mit denen Sie kompliziertere Fälle elegant meistern:

- Verwenden Sie den `CL_OSQL_REPLACE`-Service zum Testen komplexer OpenSQL-Anweisungen. Diese werden in einen Testdatenbehälter umgeleitet, der mit Testdaten befüllt werden kann, ohne den Rest des Systems zu beeinflussen.

- Verwenden Sie das CDS Test Framework zum Testen Ihrer CDS-Sichten.

#### Testseams als temporäre Behelfslösung verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#use-test-seams-as-temporary-workaround)

Wenn alle anderen Techniken fehlschlagen, oder Sie sich im gefährlich seichten Wasser von Legacy-Code befinden, greifen Sie auf [Testseams](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm)zurück, um die Dinge testbar zu machen.

Auch wenn sie auf den ersten Blick komfortabel aussehen, sind Testseams invasiv und tendieren dazu, sich in privaten Abhängigkeiten zu verheddern, so dass es langfristig schwer ist, sie am Leben und stabil zu halten.

Wir empfehlen daher, auf Testseams nur als temporäre Behelfslösung zuzugreifen, um den Code mittels Refactoring in eine besser testbare Form zu bringen.

#### Mit LOCAL FRIENDS auf Abhängigkeitsumkehr-Konstruktor zugreifen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#use-local-friends-to-access-the-dependency-inverting-constructor)

```ABAP
CLASS /clean/unit_tests DEFINITION.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /clean/interface_under_test.
    METHODS setup.
ENDCLASS.

CLASS /clean/class_under_test DEFINITION LOCAL FRIENDS unit_tests.

CLASS unit_tests IMPLEMENTATION.
  METHOD setup.
    DATA(mock) = cl_abap_testdouble=>create( '/clean/some_mock' ).
    " /clean/class_under_test is CREATE PRIVATE
     " so this only works because of the LOCAL FRIENDS
    cut = NEW /clean/class_under_test( mock ).
  ENDMETHOD.
ENDCLASS.
```

#### LOCAL FRIENDS nicht zum Eindringen in den getesteten Code missbrauchen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#dont-misuse-local-friends-to-invade-the-tested-code)

Modultests, die auf private und geschützte Mitglieder zugreifen, um Mock-Daten einzufügen, sind fragil: Sie versagen, wenn sich die interne Struktur des getesteten Codes ändert.

```ABAP
" anti-pattern
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### Produktiven Code nicht zugunsten Testbarkeit ändern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#dont-change-the-productive-code-to-make-the-code-testable)

```ABAP
" anti-pattern
IF me->in_test_mode = abap_true.
```

#### Keine Unterklassen zum Nachstellen von Methoden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#dont-sub-class-to-mock-methods)

Es wird davon abgeraten, Methodenunterklassen anzulegen und Methoden zu überschreiben, um diese in Ihren Modultests nachzustellen. Obwohl das funktioniert, ist es eine fragile Angelegenheit, weil die Tests beim Refactoring des Codes leicht funktionsunfähig gemacht werden. Außerdem erhalten reale Konsumenten dadurch die Möglichkeit, Ihre Klasse zu erben, was [Sie unvorbereitet treffen kann, wenn Sie dies nicht explizit im Design festgelegt haben](#final-if-not-designed-for-inheritance).

```ABAP
" anti-pattern
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

Um Legacy-Code unter Test zu erhalten, [greifen Sie stattdessen auf Testseams zurück](#use-test-seams-as-temporary-workaround).
Sie sind ebenso fragil, aber trotzdem die sauberere Möglichkeit, weil sie zumindest nicht das produktive Verhalten der Klasse ändern. Dies würde geschehen, wenn die Vererbung aktiviert wird, indem ein früheres `FINAL`-Kennzeichen gelöscht oder der Methoden-Scope von `PRIVATE` in `PROTECTED` geändert wird.

Beim Schreiben von neuem Code berücksichtigen Sie dieses Thema der Testbarkeit direkt beim Entwurf der Klasse, und suchen Sie nach einer anderen, besseren Vorgehensweise. Zu den gängigen bewährten Praktiken zählen die [Nutzung anderer Testtools](#exploit-the-test-tools) und die Extraktion der Problemmethode in eine separate Klasse mit ihrer eigenen Schnittstelle.

> Eine spezifischere Variante von [Produktiven Code nicht zugunsten Testbarkeit ändern](#dont-change-the-productive-code-to-make-the-code-testable).

#### Nichts Unnötiges nachstellen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#dont-mock-stuff-thats-not-needed)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

Definieren Sie Ihre Gegebenheiten („given“) so präzise wie möglich: Legen Sie keine Daten fest, die Ihr Test nicht benötigt, und stellen Sie keine Objekte nach, die nie aufgerufen werden. Diese Dinge lenken den Leser vom eigentlichen Geschehen ab.

```ABAP
" anti-pattern
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

Es kann vorkommen, dass überhaupt keine Notwendigkeit besteht, überhaupt irgendetwas nachzustellen - dies ist gewöhnlich bei Datenstrukturen und Datencontainern der Fall. So kann Ihr Modultest beispielsweise mit der produktiven Version eines `transient_log` gut funktionieren, weil dieses die Daten ohne jegliche Nebeneffekte einfach speichert.

#### Keine Test-Frameworks aufbauen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Injection](#injection) > [Dieser Abschnitt](#dont-build-test-frameworks)

Modultests - im Gegensatz zu Integrationstests - sollten auf der „Daten rein-Daten-raus-Basis“ funktionieren, während alle Testdaten bei Bedarf dynamisch definiert werden.

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

Fangen Sie nicht damit an, Frameworks zu konstruieren, die auf der Basis von „*Testfall-IDs*“ entscheiden, welche Daten bereitgestellt werden sollen. Der resultierende Code wäre so lang und kompliziert, dass diese Tests nicht langfristig nutzbar wären.

```ABAP
" anti-pattern

test_double->set_test_case( 1 ).

CASE me->test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### Testmethoden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#test-methods)

#### Testmethodennamen: was ist gegeben, was wird erwartet

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testmethoden](#test-methods) > [Dieser Abschnitt](#test-method-names-reflect-whats-given-and-expected)

Gute Namen reflektieren das „given“ (die Ausgangssituation des Tests) und das „then“ (Ergebnis ist die gewünschte Zielsituation) des Tests:

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

Schlechte Namen reflektieren das „when“ (Wenn in dieser Situation ein bestimmtes Ereignis eintritt), wiederholen bedeutungslose Fakten oder sind kryptisch:

```ABAP
" anti-patterns

" What's expected, success or failure?
METHOD get_conversion_exits.

" It's a test method, what else should it do but "test"?
METHOD test_loop.

" So it's parameterized, but what is its aim?
METHOD parameterized_test.

" What's "_wo_w" supposed to mean and will you still remember that in a year from now?
METHOD get_attributes_wo_w.
```

Da ABAP nur 30 Zeichen in Methodennamen gestattet, ist es angebracht, einen erläuternden Kommentar hinzuzufügen, wenn der Name zu kurz ist, um eine ausreichende Bedeutung zu übermitteln. ABAP Doc oder die erste Zeile der Testmethode können eine geeignete Wahl für den Kommentar sein.

Eine Menge Testmethoden, deren Namen zu lang sind, können ein Hinweis darauf sein, dass Sie Ihre einzelne Testklasse in mehrere Testklassen aufteilen und die Unterschiede im „given“ des jeweiligen Klassennamens ausdrücken sollten.

#### Given/When/Then verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testmethoden](#test-methods) > [Dieser Abschnitt](#use-given-when-then)

Organisieren Sie Ihren Testcode anhand des „Given-When-Then“-Paradigmas:
Als Erstes initialisieren das Gegebene im Abschnitt „given“.
Als Nächstes rufen Sie die tatsächlich getestete Sache auf („when“),
und im dritten Schritt validieren Sie das Ergebnis („then“).

Werden die Abschnitte „given“ oder „then“ so lang, dass Sie die drei Abschnitte nicht mehr klar auseinanderhalten können, extrahieren Sie Untermethoden.
Leerzeilen oder Kommentare zur Trennung sehen auf den ersten Blick vielleicht gut aus, beheben jedoch nicht wirklich das visuelle Wirrwarr. Trotzdem sind sie für den Leser und unerfahrene Testentwickler hilfreich beim Trennen der einzelnen Abschnitte.

#### „When“ ist genau ein Aufruf

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testmethoden](#test-methods) > [Dieser Abschnitt](#when-is-exactly-one-call)

Stellen Sie sicher, dass der „when“-Abschnitt Ihrer Testmethode genau einen Aufruf an die getestete Klasse enthält.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Der Aufruf mehrerer Dinge gleichzeitig verrät, dass die Methode keinen klaren Fokus hat und zu viele Dinge testet. Dies erschwert das Auffinden der Ursache, wenn der Test fehlschlägt: War es der erste, zweite oder dritte Aufruf, der den Fehler verursacht hat? Außerdem verwirrt es den Leser, da er nicht genau ersehen kann, welche Funktion eigentlich getestet wird.

#### TEARDOWN nur, wenn es sein muss

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testmethoden](#test-methods) > [Dieser Abschnitt](#dont-add-a-teardown-unless-you-really-need-it)

`teardown`-Methoden werden gewöhnlich nur zum Bereinigen von Datenbankeinträgen oder anderen externen Ressourcen in Integrationstests benötigt.

Das Zurücksetzen der Testklassenmitglieder, insbesondere `cut`, und der verwendeten Testattrappen ist überflüssig. Sie werden vor dem Start der nächsten Testmethode von der `setup`-Methode überschrieben.

### Testdaten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#test-data)

#### Einfach erkennbare Bedeutung

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testdaten](#test-data) > [Dieser Abschnitt](#make-it-easy-to-spot-meaning)

In Modultests möchten Sie schnell erkennen können, welche Daten und Testattrappen wichtig sind, und welchen nur dazu da sind, um einen Crash des Codes zu vermeiden. Unterstützen Sie dies, indem Sie bedeutungslosen Dingen offensichtliche Namen und Werte geben, wie z.B.:

```ABAP
DATA(alert_id) = '42'.                             " well-known meaningless numbers
DATA(detection_object_type) = '?=/"&'.             " 'keyboard accidents'
CONSTANTS some_random_number TYPE i VALUE 782346.  " revealing variable names
```

Verleiten Sie die Leser nicht zu dem Glauben, dass etwas mit realen Objekten oder realem Customizing zu tun hat, wenn dies nicht der Fall ist.

```ABAP
" anti-pattern
DATA(alert_id) = '00000001223678871'.        " this alert really exists
DATA(detection_object_type) = 'FRA_SCLAIM'.  " this detection object type, too
CONSTANTS memory_limit TYPE i VALUE 4096.    " this number looks carefully chosen
```

#### Einfach erkennbare Abweichungen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testdaten](#test-data) > [Dieser Abschnitt](#make-it-easy-to-spot-differences)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

Zwingen Sie die Leser nicht dazu, lange, bedeutungslose Strings zu vergleichen, um winzige Unterschiede herauszufinden.

#### Konstanten zur Beschreibung von Zweck und Bedeutung der Testdaten verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Testdaten](#test-data) > [Dieser Abschnitt](#use-constants-to-describe-purpose-and-importance-of-test-data)

```ABAP
CONSTANTS some_nonsense_key TYPE char8 VALUE 'ABCDEFGH'.

METHOD throws_on_invalid_entry.
  TRY.
      " when
      cut->read_entry( some_nonsense_key ).
      cl_abap_unit_assert=>fail( ).
    CATCH /clean/customizing_reader_error.
      " then
  ENDTRY.
ENDMETHOD.
```

### Assertionen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Dieser Abschnitt](#assertions)

#### Wenige, fokussierte Assertionen

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#few-focused-assertions)

Versichern Sie nur genau das, worum es in der Testmethode geht, und verwenden Sie dazu wenige Assertionen.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Zu viele Assertionen sind ein Hinweis darauf, dass die Methode keinen klaren Fokus hat. Hierdurch wird produktiver Code und Testcode an zu vielen Stellen aneinandergekoppelt: Wird eine Funktion geändert, muss eine große Anzahl von Tests umgeschrieben werden, obwohl sie nicht wirklich etwas mit der geänderten Funktion zu tun haben. Zu viele Assertionen sind außerdem für den Leser verwirrend, weil die einzige wichtige Assertion, auf die es ankommt, schwer erkennbar ist.

```ABAP
" anti-pattern
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
  cl_abap_unit_assert=>assert_not_initial( log->get_messages( ) ).
  cl_abap_unit_assert=>assert_equals( act = sy-langu
                                      exp = 'E' ).
ENDMETHOD.
```

#### Korrekten Assertionstyp verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#use-the-right-assert-type)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

Assertionen tun häufig mehr, als es auf den ersten Blick erscheint. So umfasst `assert_equals` beispielsweise die Typenähnlichkeitsfindung und stellt präzise Beschreibungen zur Verfügung, wenn Werte abweichen. Eine Verwendung von falschen, zu allgemeinen Assertionen zwingt Sie sofort in den Debugger, anstatt Ihnen die Möglichkeit zu geben, den Fehler direkt aus der Fehlermeldung zu ersehen.

```ABAP
" anti-pattern
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### Inhalt, nicht Menge zusichern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#assert-content-not-quantity)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

Schreiben Sie keine Magische-Zahlen-Mengenassertionen, wenn Sie den tatsächlichen Inhalt benennen können, den Sie erwarten. Zahlen können variieren, obwohl die Erwartungen trotzdem erfüllt werden. Umgekehrt können die Zahlen stimmen, obwohl der Inhalt etwas völlig Unerwartetes ist. 

```ABAP
" anti-pattern
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### Qualität, nicht Inhalt zusichern

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#assert-quality-not-content)

Wenn Sie an der Metaqualität des Ergebnisses interessiert sind, jedoch nicht an dem eigentlichen Inhalt, drücken Sie dies mit einer geeigneten Assertion aus:

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

Die Assertion des präzisen Inhalts verschleiert, was Sie wirklich testen möchten. Es ist außerdem eine fragile Vorgehensweise, weil durch das Refactoring möglicherweise ein anderes, jedoch völlig akzeptables Ergebnis erzielt wird, obwohl es alle Ihre zu präzisen Modultests durchbricht.

```ABAP
" anti-pattern
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### FAIL zum Prüfen erwarteter Ausnahmen verwenden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#use-fail-to-check-for-expected-exceptions)

```ABAP
METHOD throws_on_empty_input.
  TRY.
      " when
      cut->do_something( '' ).
      cl_abap_unit_assert=>fail( ).
    CATCH /clean/some_exception.
      " then
  ENDTRY.
ENDMETHOD.
```

#### Unerwartete Ausnahmen nicht vergeblich abfangen, sondern weiterleiten

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#forward-unexpected-exceptions-instead-of-catching-and-failing)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

Ihr Testcode bleibt auf den glücklichen Pfad fokussiert und ist daher sehr viel einfacher zu lesen und zu verstehen als:

```ABAP
" anti-pattern
METHOD reads_entry.
  TRY.
      DATA(entry) = cut->read_something( ).
    CATCH /clean/some_exception INTO DATA(unexpected_exception).
      cl_abap_unit_assert=>fail( unexpected_exception->get_text( ) ).
  ENDTRY.
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

#### Angepasste Assertionen: Code verkürzen, Doppeltes vermeiden

> [Clean ABAP](#clean-abap) > [Inhalt](#content) > [Test](#testing) > [Assertionen](#assertions) > [Dieser Abschnitt](#write-custom-asserts-to-shorten-code-and-avoid-duplication)

```ABAP
METHODS assert_contains
  IMPORTING
    actual_entries TYPE STANDARD TABLE OF entries_tab
    expected_key   TYPE key_structure.

METHOD assert_contains.
  TRY.
      actual_entries[ key = expected_key ].
    CATCH cx_sy_itab_line_not_found.
      cl_abap_unit_assert=>fail( |Couldn't find the key { expected_key }| ).
  ENDTRY.
ENDMETHOD.
```

Anstatt mit wiederholtem Kopieren und Einfügen zu hantieren.