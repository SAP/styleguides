> Translated from [English original on 14.11.2019](https://github.com/SAP/styleguides/tree/72ecf7fd7d41151d5bbca29020d4ec9de953db8c).
> Latest version [in English](CleanABAP.md).

# Coder proprement avec ABAP

> [**Français**](CleanABAP_fr.md)
> &nbsp;·&nbsp;
> [English](CleanABAP.md)
> &nbsp;·&nbsp;
> [中文](CleanABAP_zh.md)
> &nbsp;·&nbsp;
> [Deutsch](CleanABAP_de.md)
> &nbsp;·&nbsp;
> [日本語](CleanABAP_ja.md)

Ce guide est une adaptation de l'ouvrage de [Robert C. Martin_ Coder proprement (Clean Code)_] pour [ABAP](https://fr.wikipedia.org/wiki/ABAP).

L'[antisèche](cheat-sheet/CheatSheet.md) est une version optimisée pour l'impression.

[Robert C. Martin_ Coder proprement (Clean Code)_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## Table des matières

- [Comment faire pour](#comment-faire-pour)
   - [Démarrer avec la méthodologie Clean Code](#démarrer-avec-la-méthodologie-clean-code)
   - [Refactoriser du code existant](#refactoriser-du-code-existant)
   - [Effectuer une vérification automatique](#effectuer-une-vérification-automatique)
   - [Faire le lien avec d'autres guides](#faire-le-lien-avec-dautres-guides)
   - [Exprimer un désaccord](#exprimer-un-désaccord)
- [Noms](#noms)
   - [Utilisez des noms descriptifs](#utilisez-des-noms-descriptifs)
   - [Préférez des termes du domaine de la solution et du domaine du problème](#préférez-des-termes-du-domaine-de-la-solution-et-du-domaine-du-problème)
   - [Utilisez le pluriel](#utilisez-le-pluriel)
   - [Utilisez des noms prononçables](#utilisez-des-noms-prononçables)
   - [Évitez les abréviations](#évitez-les-abréviations)
   - [Utilisez les mêmes abréviations partout](#utilisez-les-mêmes-abréviations-partout)
   - [Utilisez des noms pour les classes, et des verbes pour les méthodes](#utilisez-des-noms-pour-les-classes-et-des-verbes-pour-les-méthodes)
   - [Évitez les mots parasites, comme "données", "info", "objet"](#évitez-les-mots-parasites-comme-données-info-objet)
   - [Sélectionnez un mot par concept](#sélectionnez-un-mot-par-concept)
   - [Utilisez des noms de modèle uniquement s'ils ont du sens](#utilisez-des-noms-de-modèle-uniquement-sils-ont-du-sens)
   - [Évitez les codifications, en particulier la notation hongroise et les préfixes](#évitez-les-codifications-en-particulier-la-notation-hongroise-et-les-préfixes)
- [Langage](#langage)
   - [Tenez compte des éléments existants](#tenez-compte-des-éléments-existants)
   - [Tenez compte des performances](#tenez-compte-des-performances)
   - [Préférez la programmation orientée objet à la programmation procédurale](#préférez-la-programmation-orientée-objet-à-la-programmation-procédurale)
   - [Préférez des constructs de langage fonctionnel plutôt que procédural](#préférez-des-constructs-de-langage-fonctionnel-plutôt-que-procédural)
   - [Évitez les éléments de langage obsolètes](#évitez-les-éléments-de-langage-obsolètes)
   - [Utilisez les modèles de conception de manière avisée](#utilisez-les-modèles-de-conception-de-manière-avisée)
- [Constantes](#constantes)
   - [Utilisez des constantes et non des nombres magiques](#utilisez-des-constantes-et-non-des-nombres-magiques)
   - [Préférez les classes d'énumération aux interfaces de constantes](#préférez-les-classes-dénumération-aux-interfaces-de-constantes)
   - [Si vous n'utilisez pas de classes d'énumération, regroupez vos constantes](#si-vous-nutilisez-pas-de-classes-dénumération-regroupez-vos-constantes)
- [Variables](#variables)
   - [Préférez les déclarations en ligne aux déclarations initiales](#préférez-les-déclarations-en-ligne-aux-déclarations-initiales)
   - [N'effectuez pas de déclarations en ligne dans les branchements facultatifs](#neffectuez-pas-de-déclarations-en-ligne-dans-les-branchements-facultatifs)
   - [Ne créez pas des chaînes de déclarations initiales](#ne-créez-pas-des-chaînes-de-déclarations-initiales)
   - [Préférez REF TO à FIELD-SYMBOL](#préférez-ref-to-à-field-symbol)
- [Tables](#tables)
   - [Utilisez le bon type de table](#utilisez-le-bon-type-de-table)
   - [Évitez DEFAULT KEY](#évitez-default-key)
   - [Préférez INSERT INTO TABLE à APPEND TO](#préférez-insert-into-table-à-append-to)
   - [Préférez LINE_EXISTS à READ TABLE ou LOOP AT](#préférez-line_exists-à-read-table-ou-loop-at)
   - [Préférez READ TABLE à LOOP AT](#préférez-read-table-à-loop-at)
   - [Préférez LOOP AT WHERE à un IF imbriqué](#préférez-loop-at-where-à-un-if-imbriqué)
   - [Évitez les lectures de table superflues](#évitez-les-lectures-de-table-superflues)
- [Chaînes de caractères](#chaînes-de-caractères)
   - [Utilisez \` pour définir des littéraux](#utilisez--pour-définir-des-littéraux)
   - [Utilisez | pour assembler du texte](#utilisez--pour-assembler-du-texte)
- [Booléens](#booléens)
   - [Utilisez les booléens de manière avisée](#utilisez-les-booléens-de-manière-avisée)
   - [Utilisez ABAP_BOOL pour les booléens](#utilisez-abap_bool-pour-les-booléens)
   - [Utilisez ABAP_TRUE et ABAP_FALSE pour les comparaisons](#utilisez-abap_true-et-abap_false-pour-les-comparaisons)
   - [Utilisez XSDBOOL pour définir des variables booléennes](#utilisez-xsdbool-pour-définir-des-variables-booléennes)
- [Conditions](#conditions)
   - [Essayez de rendre les conditions positives](#essayez-de-rendre-les-conditions-positives)
   - [Préférez IS NOT à NOT IS](#préférez-is-not-à-not-is)
   - [Efforcez-vous de décomposer les conditions complexes](#efforcez-vous-de-décomposer-les-conditions-complexes)
   - [Efforcez-vous d'extraire les conditions complexes](#efforcez-vous-dextraire-les-conditions-complexes)
- [If](#if)
   - [Aucun branchement IF vide](#aucun-branchement-if-vide)
   - [Préférez CASE à ELSE IF pour des conditions alternatives multiples](#préférez-case-à-else-if-pour-des-conditions-alternatives-multiples)
   - [Gardez un niveau d'imbrication bas](#gardez-un-niveau-dimbrication-bas)
- [Expressions régulières](#expressions-régulières)
   - [Préférez des méthodes plus simples aux expressions régulières](#préférez-des-méthodes-plus-simples-aux-expressions-régulières)
   - [Préférez les vérifications de base aux expressions régulières](#préférez-les-vérifications-de-base-aux-expressions-régulières)
   - [Efforcez-vous d'assembler des expressions régulières complexes](#efforcez-vous-dassembler-des-expressions-régulières-complexes)
- [Classes](#constructeurs)
   - [Classes : orientation objet](#classes--orientation-objet)
      - [Préférez les objets aux classes statiques](#préférez-les-objets-aux-classes-statiques)
      - [Préférez la composition à l'héritage](#préférez-la-composition-à-lhéritage)
      - [Ne mélangez pas des paradigmes avec statut et sans statut dans la même classe](#ne-mélangez-pas-des-paradigmes-avec-statut-et-sans-statut-dans-la-même-classe)
   - [Portée](#portée)
      - [Globale par défaut, locale uniquement le cas échéant](#globale-par-défaut-locale-uniquement-le-cas-échéant)
      - [FINAL si non conçue pour l'héritage](#final-si-non-conçue-pour-lhéritage)
      - [Membres PRIVATE par défaut, PROTECTED seulement si besoin](#membres-private-par-défaut-protected-seulement-si-besoin)
      - [Efforcez-vous d'utiliser un immuable au lieu d'un getter](#efforcez-vous-dutiliser-un-immuable-au-lieu-dun-getter)
      - [Utilisez READ-ONLY avec parcimonie](#utilisez-read-only-avec-parcimonie)
   - [Constructeurs](#constructeurs)
      - [Préférez NEW à CREATE OBJECT](#préférez-new-à-create-object)
      - [Si votre classe globale est CREATE PRIVATE, laissez CONSTRUCTOR public](#si-votre-classe-globale-est-create-private-laissez-constructor-public)
      - [Préférez les méthodes de création statiques multiples aux paramètres facultatifs](#préférez-les-méthodes-de-création-statiques-multiples-aux-paramètres-facultatifs)
      - [Utilisez des noms descriptifs pour les méthodes de création multiples](#utilisez-des-noms-descriptifs-pour-les-méthodes-de-création-multiples)
      - [Codez des singletons uniquement là où les instances multiples n'ont pas de sens](#codez-des-singletons-uniquement-là-où-les-instances-multiples-nont-pas-de-sens)
- [Méthodes](#méthodes)
   - [Appels](#appels)
      - [Préférez les appels fonctionnels aux appels procéduraux](#préférez-les-appels-fonctionnels-aux-appels-procéduraux)
      - [Omettez RECEIVING](#omettez-receiving)
      - [Omettez le mot-clé facultatif EXPORTING](#omettez-le-mot-clé-facultatif-exporting)
      - [Omettez le nom du paramètre dans les appels de paramètre unique](#omettez-le-nom-du-paramètre-dans-les-appels-de-paramètre-unique)
      - [Omettez l'autoréférence me lorsque vous appelez une méthode d'instance](#omettez-lautoréférence-me-lorsque-vous-appelez-une-méthode-dinstance)
   - [Méthodes : orientation objet](#méthodes--orientation-objet)
      - [Préférez les méthodes d'instance aux méthodes statiques](#préférez-les-méthodes-dinstance-aux-méthodes-statiques)
      - [Les méthodes d'instance publiques doivent faire partie d'une interface](#les-méthodes-dinstance-publiques-doivent-faire-partie-dune-interface)
   - [Nombre de paramètres](#nombre-de-paramètres)
      - [Visez un petit nombre de paramètres IMPORTING, dans l'idéal moins de trois](#visez-un-petit-nombre-de-paramètres-importing-dans-lidéal-moins-de-trois)
      - [Fractionnez les méthodes au lieu d'ajouter des paramètres OPTIONAL](#fractionnez-les-méthodes-au-lieu-dajouter-des-paramètres-optional)
      - [Utilisez PREFERRED PARAMETER avec parcimonie](#utilisez-preferred-parameter-avec-parcimonie)
      - [Utilisez RETURN, EXPORT ou CHANGE pour un seul et unique paramètre](#utilisez-return-export-ou-change-pour-un-seul-et-unique-paramètre)
   - [Types de paramètre](#types-de-paramètre)
      - [Préférez RETURNING à EXPORTING](#préférez-returning-à-exporting)
      - [N'hésitez pas à utiliser RETURNING avec de grandes tables](#nhésitez-pas-à-utiliser-returning-avec-de-grandes-tables)
      - [Utilisez soit RETURNING, soit EXPORTING, soit CHANGING, mais ne les utilisez pas en combinaison](#utilisez-soit-returning-soit-exporting-soit-changing-mais-ne-les-utilisez-pas-en-combinaison)
      - [Utilisez CHANGING avec parcimonie, lorsque cela est adapté](#utilisez-changing-avec-parcimonie-lorsque-cela-est-adapté)
      - [Fractionnez la méthode au lieu d'utiliser un paramètre d'entrée booléen](#fractionnez-la-méthode-au-lieu-dutiliser-un-paramètre-dentrée-booléen)
   - [Noms de paramètres](#noms-de-paramètres)
      - [Efforcez-vous d'appeler "RESULT" le paramètre RETURNING](#efforcez-vous-dappeler-result-le-paramètre-returning)
   - [Initialisation de paramètres](#initialisation-de-paramètres)
      - [Réinitialisez ou écrasez les paramètres de référence EXPORTING](#réinitialisez-ou-écrasez-les-paramètres-de-référence-exporting)
         - [Soyez vigilant si l'entrée et la sortie peuvent être identiques](#soyez-vigilant-si-lentrée-et-la-sortie-peuvent-être-identiques)
      - [Ne réinitialisez pas les paramètres VALUE](#ne-réinitialisez-pas-les-paramètres-value)
   - [Corps de la méthode](#corps-de-la-méthode)
      - [Faites une chose, faites-la bien et ne faites que cela](#faites-une-chose-faites-la-bien-et-ne-faites-que-cela)
      - [Activez les cas d'utilisation correcte ou la gestion des erreurs, mais pas les deux](#activez-les-cas-dutilisation-correcte-ou-la-gestion-des-erreurs-mais-pas-les-deux)
      - [Descendez d'un niveau d'abstraction](#descendez-dun-niveau-dabstraction)
      - [Privilégiez les méthodes courtes](#privilégiez-les-méthodes-courtes)
   - [Flux de contrôle](#flux-de-contrôle)
      - [Échec accéléré](#échec-accéléré)
      - [CHECK contre RETURN](#check-contre-return)
      - [Évitez d'utiliser CHECK dans d'autres positions](#évitez-dutiliser-check-dans-dautres-positions)
- [Gestion des erreurs](#gestion-des-erreurs)
   - [Messages](#messages)
      - [Facilitez la recherche des messages](#facilitez-la-recherche-des-messages)
   - [Codes retour](#codes-retour)
      - [Préférez les exceptions aux codes retour](#préférez-les-exceptions-aux-codes-retour)
      - [Ne laissez pas passer des erreurs](#ne-laissez-pas-passer-des-erreurs)
   - [Exceptions](#exceptions)
      - [Les exceptions sont pour les erreurs, et non pour les cas normaux](#les-exceptions-sont-pour-les-erreurs-et-non-pour-les-cas-normaux)
      - [Utilisez des exceptions basées sur une classe](#utilisez-des-exceptions-basées-sur-une-classe)
   - [Levée d'exceptions](#levée-dexceptions)
      - [Utilisez vos propres surclasses](#utilisez-vos-propres-surclasses)
      - [Levez un seul type d'exception](#levez-un-seul-type-dexception)
      - [Utilisez des surclasses pour permettre aux programmes appelant d'identifier les situations d'erreur](#utilisez-des-surclasses-pour-permettre-aux-programmes-appelant-didentifier-les-situations-derreur)
      - [Levez CX_STATIC_CHECK pour les exceptions gérables](#levez-cx_static_check-pour-les-exceptions-gérables)
      - [Levez CX_NO_CHECK pour les situations généralement irrécupérables](#levez-cx_no_check-pour-les-situations-généralement-irrécupérables)
      - [Pensez à CX_DYNAMIC_CHECK pour les exceptions évitables](#pensez-à-cx_dynamic_check-pour-les-exceptions-évitables)
      - [Procédez à un vidage de la mémoire pour les situations totalement irrécupérables](#procédez-à-un-vidage-de-la-mémoire-pour-les-situations-totalement-irrécupérables)
      - [Préférez RAISE EXCEPTION NEW à RAISE EXCEPTION TYPE](#préférez-raise-exception-new-à-raise-exception-type)
   - [Interception](#interception)
      - [Enveloppez les exceptions externes pour éviter qu'elles n'envahissent votre code](#enveloppez-les-exceptions-externes-pour-éviter-quelles-nenvahissent-votre-code)
- [Commentaires](#commentaires)
   - [Exprimez-vous via le code, et non via des commentaires](#exprimez-vous-via-le-code-et-non-via-des-commentaires)
   - [Les commentaires ne sont pas une excuse devant justifier les noms inappropriés](#les-commentaires-ne-sont-pas-une-excuse-devant-justifier-les-noms-inappropriés)
   - [Utilisez les méthodes plutôt que les commentaires pour segmenter votre code](#utilisez-les-méthodes-plutôt-que-les-commentaires-pour-segmenter-votre-code)
   - [Écrivez des commentaires pour répondre à la question "pourquoi ?", et non à la question "quoi ?"](#écrivez-des-commentaires-pour-répondre-à-la-question-pourquoi--et-non-à-la-question-quoi-)
   - [La conception doit être traitée dans les documents de conception, et non dans le code](#la-conception-doit-être-traitée-dans-les-documents-de-conception-et-non-dans-le-code)
   - [Commentez avec \", et non avec \*](#commentez-avec--et-non-avec-)
   - [Mettez des commentaires avant l'instruction à laquelle ils font référence](#mettez-des-commentaires-avant-linstruction-à-laquelle-ils-font-référence)
   - [Supprimez le code au lieu de le commenter](#supprimez-le-code-au-lieu-de-le-commenter)
   - [Utilisez FIXME, TODO et XXX, et ajoutez votre ID](#utilisez-fixme-todo-et-xxx-et-ajoutez-votre-id)
   - [N'ajoutez pas de commentaires de type signature de méthode et end-of](#najoutez-pas-de-commentaires-de-type-signature-de-méthode-et-end-of)
   - [N'ajoutez pas des commentaires qui font doublons avec les textes de messages](#najoutez-pas-des-commentaires-qui-font-doublons-avec-les-textes-de-messages)
   - [Utilisez ABAP Doc uniquement pour les API publiques](#utilisez-abap-doc-uniquement-pour-les-api-publiques)
   - [Préférez les pragmas aux pseudo-commentaires](#préférez-les-pragmas-aux-pseudo-commentaires)
- [Mise en forme](#mise-en-forme)
   - [Soyez cohérent](#soyez-cohérent)
   - [Optimisez votre code pour la lecture, et non l'écriture](#optimisez-votre-code-pour-la-lecture-et-non-lécriture)
   - [Utilisez la fonction Pretty Printer avant l'activation](#utilisez-la-fonction-pretty-printer-avant-lactivation)
   - [Utilisez les options Pretty Printer paramétrées pour votre équipe](#utilisez-les-options-pretty-printer-paramétrées-pour-votre-équipe)
   - [Pas plus d'une instruction par ligne](#pas-plus-dune-instruction-par-ligne)
   - [Gardez une longueur de ligne raisonnable](#gardez-une-longueur-de-ligne-raisonnable)
   - [Condensez votre code](#condensez-votre-code)
   - [Ajoutez juste une ligne vierge pour séparer les différentes parties, pas plus](#ajoutez-juste-une-ligne-vierge-pour-séparer-les-différentes-parties-pas-plus)
   - [N'abusez pas des lignes vierges de séparation](#nabusez-pas-des-lignes-vierges-de-séparation)
   - [Alignez les affectations sur le même objet, et non sur des objets différents](#alignez-les-affectations-sur-le-même-objet-et-non-sur-des-objets-différents)
   - [Fermez les guillemets à la fin de la ligne](#fermez-les-guillemets-à-la-fin-de-la-ligne)
   - [Faites en sorte que les appels de paramètre unique soient sur une seule ligne](#faites-en-sorte-que-les-appels-de-paramètre-unique-soient-sur-une-seule-ligne)
   - [Faites en sorte que les paramètres apparaissent derrière l'appel](#faites-en-sorte-que-les-paramètres-apparaissent-derrière-lappel)
   - [Si vous revenez à la ligne, mettez les paramètres en retrait sous l'appel](#si-vous-revenez-à-la-ligne-mettez-les-paramètres-en-retrait-sous-lappel)
   - [En cas de multiples paramètres, revenez à la ligne](#en-cas-de-multiples-paramètres-revenez-à-la-ligne)
   - [Alignez les paramètres](#alignez-les-paramètres)
   - [Mettez l'appel sur deux lignes si la ligne initiale est trop longue](#mettez-lappel-sur-deux-lignes-si-la-ligne-initiale-est-trop-longue)
   - [Ajoutez un retrait et passez-le en tabulation](#ajoutez-un-retrait-et-passez-le-en-tabulation)
   - [Mettez les déclarations en ligne en retrait comme les appels de méthode](#mettez-les-déclarations-en-ligne-en-retrait-comme-les-appels-de-méthode)
   - [N'alignez pas les clauses type](#nalignez-pas-les-clauses-type)
- [Test](#test)
   - [Principes](#principes)
      - [Écrivez des codes testables](#écrivez-des-codes-testables)
      - [Laissez les autres faire des simulations de votre code](#laissez-les-autres-faire-des-simulations-de-votre-code)
      - [Règles relatives à la lisibilité](#règles-relatives-à-la-lisibilité)
      - [Ne faites pas de copies et n'écrivez pas de programmes de test](#ne-faites-pas-de-copies-et-nécrivez-pas-de-programmes-de-test)
      - [Testez les parties publiques, et non les parties internes privées](#testez-les-parties-publiques-et-non-les-parties-internes-privées)
      - [Ne soyez pas obsédé par la couverture du code](#ne-soyez-pas-obsédé-par-la-couverture-du-code)
   - [Classes de test](#classes-de-test)
      - [Appelez les classes de test locales en fonction de leur objectif](#appelez-les-classes-de-test-locales-en-fonction-de-leur-objectif)
      - [Mettez les tests dans les classes locales](#mettez-les-tests-dans-les-classes-locales)
      - [Mettez les méthodes d'aide dans les classes d'aide](#mettez-les-méthodes-daide-dans-les-classes-daide)
      - [Comment exécuter des classes de test](#comment-exécuter-des-classes-de-test)
   - [Membre testé](#membre-testé)
      - [Donnez un nom explicite au membre testé ou dénommez-le CUT par défaut](#donnez-un-nom-explicite-au-membre-testé-ou-dénommez-le-cut-par-défaut)
      - [Testez des interfaces, pas des classes](#testez-des-interfaces-pas-des-classes)
      - [Extrayez l'appel au membre testé dans sa propre méthode](#extrayez-lappel-au-membre-testé-dans-sa-propre-méthode)
   - [Injection](#injection)
      - [Utilisez l'inversion des dépendances pour injecter des simulations test](#utilisez-linversion-des-dépendances-pour-injecter-des-simulations-test)
      - [Pensez à utiliser l'outil simulation test ABAP](#pensez-à-utiliser-loutil-simulation-test-abap)
      - [Exploitez les outils de test](#exploitez-les-outils-de-test)
      - [Utilisez les test seams comme solution de contournement temporaire](#utilisez-les-test-seams-comme-solution-de-contournement-temporaire)
      - [Utilisez LOCAL FRIENDS pour accéder au constructeur d'inversion des dépendances](#utilisez-local-friends-pour-accéder-au-constructeur-dinversion-des-dépendances)
      - [N'utilisez pas LOCAL FRIENDS à mauvais escient pour envahir le code testé](#nutilisez-pas-local-friends-à-mauvais-escient-pour-envahir-le-code-testé)
      - [Ne modifiez pas le code productif pour rendre le code testable](#ne-modifiez-pas-le-code-productif-pour-rendre-le-code-testable)
      - [Ne créez pas des sous-classes pour simuler des méthodes](#ne-créez-pas-des-sous-classes-pour-simuler-des-méthodes)
      - [Ne simulez pas ce dont vous n'avez pas besoin](#ne-simulez-pas-ce-dont-vous-navez-pas-besoin)
      - [Ne créez pas de framework de test](#ne-créez-pas-de-framework-de-test)
   - [Méthodes de test](#méthodes-de-test)
      - [Noms de méthode de test : reflètent ce qui est donné et attendu](#noms-de-méthode-de-test--reflètent-ce-qui-est-donné-et-attendu)
      - [Utilisez le format given-when-then](#utilisez-le-format-given-when-then)
      - ["When" est un appel et un seul](#when-est-un-appel-et-un-seul)
      - [N'ajoutez pas un TEARDOWN à moins d'en avoir vraiment besoin](#najoutez-pas-un-teardown-à-moins-den-avoir-vraiment-besoin)
   - [Données de test](#données-de-test)
      - [Facilitez la lecture et la compréhension](#facilitez-la-lecture-et-la-compréhension)
      - [Faites ressortir les différences](#faites-ressortir-les-différences)
      - [Utilisez des constantes pour décrire l'objectif et l'importance des données de test](#utilisez-des-constantes-pour-décrire-lobjectif-et-limportance-des-données-de-test)
   - [Assertions](#assertions)
      - [Des assertions en faible nombre, ciblées](#des-assertions-en-faible-nombre-ciblées)
      - [Utilisez le bon type d'assert](#utilisez-le-bon-type-dassert)
      - [Utilisez les asserts pour du contenu, pas pour de la quantité](#utilisez-les-asserts-pour-du-contenu-pas-pour-de-la-quantité)
      - [Utilisez les asserts pour de la qualité, pas pour du contenu](#utilisez-les-asserts-pour-de-la-qualité-pas-pour-du-contenu)
      - [Utilisez FAIL pour rechercher les exceptions attendues](#utilisez-fail-pour-rechercher-les-exceptions-attendues)
      - [Transmettez les exceptions inattendues au lieu d'utiliser catch et fail](#transmettez-les-exceptions-inattendues-au-lieu-dutiliser-catch-et-fail)
      - [Écrivez des asserts personnalisés pour raccourcir le code et éviter la double saisie](#écrivez-des-asserts-personnalisés-pour-raccourcir-le-code-et-éviter-la-double-saisie)

## Comment faire pour

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#comment-faire-pour)

### Démarrer avec la méthodologie Clean Code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Comment faire pour](#comment-faire-pour) > [Cette section](#démarrer-avec-la-méthodologie-clean-code)

Si vous découvrez la méthodologie Clean Code, vous devriez commencer par lire l'ouvrage de [Robert C. Martin_ Coder proprement (Clean Code)_]. L'[initiative Clean Code Developer](https://clean-code-developer.com/) peut vous aider à démarrer avec une présentation générale, qui se veut didactique en abordant le sujet pas à pas.

Nous vous recommandons de démarrer avec des choses faciles à comprendre et largement acceptées, comme les [booléens](#booléens), les [conditions](#conditions), et les [If](#if).

Vous tirerez probablement le plus grand profit de la section [Méthodes](#méthodes), plus particulièrement [Faites une chose, faites-la bien et ne faites que cela](#faites-une-chose-faites-la-bien-et-ne-faites-que-cela) et [Court](#privilégiez-les-méthodes-courtes), car cela améliore considérablement la structure générale de votre code.

Certains sujets ici peuvent déclencher des discussions ardues dans les équipes professionnelles qui sont expérimentées, mais novices en Clean Code ; ces sujets sont parfaitement "sains" mais il se peut que les collaborateurs aient des difficultés à s'y faire au début.

Passez à ces sujets plus controversés plus tard ; en particulier, [Commentaires](#commentaires), [Noms](#noms) et [Mise en forme](#mise-en-forme) peuvent entraîner des quasi-guerres de religion et devraient s'adresser uniquement aux équipes qui ont déjà éprouvé les effets positifs du Clean Code.

### Refactoriser du code existant

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Comment faire pour](#comment-faire-pour) > [Cette section](#refactoriser-du-code-existant)

Les sujets [Booléens](#booléens), [Conditions](#conditions), [If](#if), et [Méthodes](#méthodes) sont les plus bénéfiques si vous travaillez sur un projet existant, avec pléthore de code que vous ne pouvez pas ou que vous ne souhaitez pas modifier, car ils peuvent s'appliquer à du nouveau code sans créer de conflits.

Le sujet [Noms](#noms) est très exigeant pour les projets existants, car il peut introduire une brèche entre l'ancien code et le nouveau, à un tel point qu'il vaut mieux ignorer les sections telles que [Évitez les codifications, en particulier la notation hongroise et les préfixes](#évitez-les-codifications-en-particulier-la-notation-hongroise-et-les-préfixes).

Nous avons constaté de bons résultats avec un plan de refactoring en quatre étapes :

1. Embarquez l'équipe avec vous. Communiquez et expliquez le nouveau style, et amenez tout le monde dans l'équipe projet à y adhérer. Vous n'avez pas besoin de suivre toutes les directives d'un seul coup, commencez par un petit sous-ensemble incontesté, puis évoluez à partir de là.

2. Suivez la _règle du boy-scout_ pour votre travail quotidien : _laissez toujours le code que vous traitez un peu plus propre que vous l'avez trouvé_. Ne vous obsédez pas à passer des heures et des heures à "nettoyer le camp", passez seulement quelques minutes à observer comment les progrès se cumulent dans le temps.

3. Créez des _îlots propres_ : de temps en temps, prenez un petit objet ou composant et essayez de le rendre propre dans tous ses aspects. Ces îlots montrent le bienfait de ce que vous faites et constituent des bases solidement testées pour un refactoring ultérieur.

4. Parlez-en. Que vous mettiez en place une [revue de code Fagan](https://en.wikipedia.org/wiki/Fagan_inspection) à l'ancienne, que vous teniez des sessions d'information ou que vous créiez un forum de discussion dans votre outil de discussion instantanée favori, vous aurez besoin de parler de vos expériences et de vos découvertes pour permettre à l'équipe d'acquérir une vision commune.

### Effectuer une vérification automatique

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Comment faire pour](#comment-faire-pour) > [Cette section](#effectuer-une-vérification-automatique)

Il n'existe pas de suite complète de contrôles de code statiques qui détecte automatiquement les contre-modèles que nous décrivons ici.

ABAP Test Cockpit, Code Inspector, Extended Check et CheckMan fournissent des contrôles qui peuvent vous aider à trouver certains problèmes.

[abapOpenChecks](https://github.com/larshp/abapOpenChecks), une collection Open Source de contrôles Code Inspector, couvent également certains des contre-modèles décrits.

[abaplint](https://github.com/abaplint/abaplint) est une réimplémentation Open Source de l'analyseur ABAP. Il fonctionne sans système SAP et il est conçu pour être utilisé sur du code sérialisé avec abapGit. Il propose de multiples intégrations (actions GitHub, Jenkins, éditeurs de texte...), couvre certains contre-modèles et peut également être utilisé pour contrôler la mise en forme et les conventions de code.

### Faire le lien avec d'autres guides

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Comment faire pour](#comment-faire-pour) > [Cette section](#faire-le-lien-avec-dautres-guides)

Notre guide suit l'_esprit_ de la méthodologie Clean Code, ce qui veut dire que nous avons adapté certains points au langage de programmation ABAP, par exemple [Levez CX_STATIC_CHECK pour les exceptions gérables](#levez-cx_static_check-pour-les-exceptions-gérables).

Certains éléments sont issus des [Directives de programmation ABAP](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm), avec lesquelles ce guide est largement compatible ; les écarts sont signalés et vont toujours dans le sens d'un code plus propre.

Ce guide respecte également les [Recommandations de la DSAG pour le développement ABAP](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf), mais nous sommes plus précis dans grand nombre de détails.

### Exprimer un désaccord

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Comment faire pour](#comment-faire-pour) > [Cette section](#exprimer-un-désaccord)

Nous avons écrit ce guide de style pour les lecteurs qui sont déjà familiarisés avec la méthodologie Clean Code ou qui sont d'ores et déjà en train d'y travailler, en nous concentrant fortement sur la manière d'appliquer le Clean Code _spécifiquement à ABAP_.

Gardez à l'esprit que de ce fait, nous n'avons pas présenté tous les concepts aussi longuement et en profondeur que dans le livre original et les ressources liées : ceux-ci valent toujours la peine d'être lus, en particulier si vous êtes en désaccord avec certains points ici seulement parce que nous ne les avons pas très bien expliqués. Utilisez les liens dans les sections pour consulter le contexte de notre prescription.

Vous êtes libre de discuter et d'exprimer votre désaccord avec tout ce que nous disons ici. La méthodologie Clean Code repose sur l'un des piliers suivants : _l'équipe établit les règles_. Assurez-vous seulement de donner une chance équitable à quelque chose avant de le rejeter.

[CONTRIBUTING.md](../CONTRIBUTING.md) suggère des façons dont vous pouvez modifier ce guide ou en dévier dans des détails mineurs.

## Noms

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#noms)

### Utilisez des noms descriptifs

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#utilisez-des-noms-descriptifs)

Utilisez des noms qui sont adaptés au contenu et à la signification des éléments.

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

Ne vous concentrez par sur le type de données ou sur la codification technique. Ils ne contribuent pas vraiment à la compréhension du code.

```ABAP
" anti-pattern
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[N'essayez pas de compenser des noms inappropriés par des commentaires.](#les-commentaires-ne-sont-pas-une-excuse-devant-justifier-les-noms-inappropriés)

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : choisir des noms révélateurs des intentions_ dans le livre de [Robert C. Martin _Coder proprement_].

### Préférez des termes du domaine de la solution et du domaine du problème

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#préférez-des-termes-du-domaine-de-la-solution-et-du-domaine-du-problème)

Recherchez des noms appropriés dans le domaine de la solution, par exemple des termes informatiques comme "file d'attente" ou "arborescence", et dans le domaine du problème traité, par exemple des termes commerciaux comme "compte" ou "ledger".

Le nom des couches à teneur commerciale sonnera mieux si elles sont intitulées d'après le domaine du problème. Cela est particulièrement vrai pour les composants qui sont conçus avec la conception pilotée par le domaine, tels que les API et les objets de gestion.

Le nom des couches qui offrent des fonctionnalités en grande partie techniques, telles que les classes de factory et les algorithmes abstraits, sonnera mieux si elles sont intitulées d'après le domaine de la solution.

Dans tous les cas, ne tentez pas de composer votre propre langage. Nous devons être en mesure d'échanger des informations entre développeurs, responsables produit, partenaires et clients. Par conséquent, choisissez des noms qui parleront à tous ces collaborateurs, sans qu'ils aient besoin de consulter un dictionnaire spécialisé.

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : choisir des noms dans le domaine de la solution_ et _[...] : > choisir des noms dans le domaine du problème_ du livre de [Robert C. Martin _Coder proprement_].

### Utilisez le pluriel

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#utilisez-le-pluriel)

La pratique existante chez SAP consiste à nommer les tables d'éléments au singulier, par exemple `country` pour une "table de pays". Hors de SAP, la tendance couramment suivie consiste à utiliser le pluriel pour les listes d'éléments. Par conséquent, nous vous recommandons de privilégier `countries` plutôt.

> Ce conseil concerne principalement les éléments tels que les variables et les propriétés. > Pour les objets de développement, il peut y avoir des modèles opposés > qui ont également du sens, par exemple, la convention largement utilisée > qui invite à nommer les tables de base de données ("tables transparentes") au singulier.

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : choisir des noms révélateurs des intentions_ dans le livre de [Robert C. Martin _Coder proprement_].

### Utilisez des noms prononçables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#utilisez-des-noms-prononçables)

Nous pensons beaucoup aux objets et en parlons beaucoup. Par conséquent, utilisez des noms que vous pouvez prononcer. Par exemple, préférez `detection_object_types` à quelque chose d'énigmatique comme `dobjt`.

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : choisir des noms prononçables_ dans le livre de [Robert C. Martin _Coder proprement_].

### Évitez les abréviations

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#évitez-les-abréviations)

Si vous avez assez d'espace, écrivez les noms complets. Commencez à abréger uniquement si vous dépassez les limites de longueur.

Si vous devez abréger, commencez par les mots _futiles_.

À première vue, cela peut sembler efficace d'abréger des éléments, mais bien vite, le contenu devient ambigu. Par exemple, est-ce que `cust` signifie "customizing" (personnalisation), "customer" (client) ou "custom" (personnalisé) ? Ces trois termes sont couramment utilisés dans les applications SAP.

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : faire des distinctions significatives_ dans le livre de [Robert C. Martin _Coder proprement_].

### Utilisez les mêmes abréviations partout

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#utilisez-les-mêmes-abréviations-partout)

Les collaborateurs rechercheront des mots-clés pour trouver du code pertinent. Facilitez-leur la tâche en utilisant la même abréviation pour le même élément. Par exemple, abrégez toujours l'abréviation "dobjt" pour "detection object type", plutôt qu'un mélange entre "dot", "dotype", "detobjtype" et ainsi de suite.

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : choisir des noms compatibles avec une recherche_ dans le livre de [Robert C. Martin _Coder proprement_].

### Utilisez des noms pour les classes, et des verbes pour les méthodes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#utilisez-des-noms-pour-les-classes-et-des-verbes-pour-les-méthodes)

Utilisez des noms ou groupes nominaux pour nommer les classes, interfaces et objets :

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

Utilisez des verbes ou groupes verbaux pour nommer les méthodes :

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

Nommez les méthodes booléennes en commençant par des verbes comme `is_` et `has_` pour rendre la lecture plus fluide :

```ABAP
IF is_empty( table ).
```

Nous vous recommandons de nommer les fonctions comme les méthodes :

```ABAP
FUNCTION /clean/read_alerts
```

### Évitez les mots parasites, comme "données", "info", "objet"

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#évitez-les-mots-parasites-comme-données-info-objet)

Omettez les mots parasites

```ABAP
account  " instead of account_data
alert    " instead of alert_object
```

ou remplacez-les par des termes spécifiques qui leur donnent vraiment du sens.

```ABAP
user_preferences          " instead of user_info
response_time_in_seconds  " instead of response_time_variable
```

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : faire des distinctions significatives_ dans le livre de [Robert C. Martin _Coder proprement_].

### Sélectionnez un mot par concept

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#sélectionnez-un-mot-par-concept)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

Choisissez un terme par concept et adhérez à cette règle. N'ajoutez pas des synonymes. En présence de synonymes, le lecteur perdra du temps à rechercher une différence, alors qu'il n'y en a pas.

```ABAP
" anti-pattern
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : choisir un mot par concept_ dans le livre de [Robert C. Martin _Coder proprement_]

### Utilisez des noms de modèle uniquement s'ils ont du sens

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#utilisez-des-noms-de-modèle-uniquement-sils-ont-du-sens)

N'utilisez pas des noms de modèles de conception logicielle pour les classes et interfaces, sauf s'ils ont vraiment ce sens. Par exemple, appelez votre classe `file_factory` uniquement si elle implémente véritablement le modèle de conception de factory. Les modèles les plus couramment utilisés incluent : [singleton](https://en.wikipedia.org/wiki/Singleton_pattern), [factory](https://en.wikipedia.org/wiki/Factory_method_pattern), [façade](https://en.wikipedia.org/wiki/Facade_pattern), [composite](https://en.wikipedia.org/wiki/Composite_pattern), [décorateur](https://en.wikipedia.org/wiki/Decorator_pattern), [itérateur](https://en.wikipedia.org/wiki/Iterator_pattern), [observateur](https://en.wikipedia.org/wiki/Observer_pattern) et [stratégie](https://en.wikipedia.org/wiki/Strategy_pattern).

> Pour en savoir plus, lisez _Chapitre 2 : Noms significatifs : éviter la désinformation_ dans le livre de [Robert C. Martin _Coder proprement_].

### Évitez les codifications, en particulier la notation hongroise et les préfixes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Noms](#noms) > [Cette section](#évitez-les-codifications-en-particulier-la-notation-hongroise-et-les-préfixes)

Nous vous encourageons à vous débarrasser de _tous_ les préfixes de codification.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

au lieu de la version inutilement plus longue

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> [Évitez les codifications](sub-sections/AvoidEncodings.md) > explique en profondeur ce raisonnement.

## Langage

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#langage)

### Tenez compte des éléments existants

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Langage](#langage) > [Cette section](#tenez-compte-des-éléments-existants)

Si vous codez pour des versions ABAP plus anciennes, prenez avec des pincettes les conseils dans ce guide : de nombreuses recommandations ci-dessous utilisent une syntaxe et des constructs relativement nouveaux qui ne sont pas nécessairement pris en charge dans les versions ABAP plus anciennes. Validez les directives à suivre sur l'ancienne version que vous devez prendre en charge. Ne rejetez pas la méthodologie Clean Code en bloc : la grande majorité des règles (par ex. : attribution de noms, commentaires) fonctionneront dans _n'importe quelle_ version ABAP.

### Tenez compte des performances

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Langage](#langage) > [Cette section](#tenez-compte-des-performances)

Si vous codez des composants hautes performances, prenez avec des pincettes les conseils dans ce guide : certains aspects du Clean Code peuvent ralentir l'exécution (un nombre plus important d'appels de méthode) ou consommez plus de mémoire (un nombre plus important d'objets). ABAP présente quelques spécificités qui peuvent intensifier le phénomène. Par exemple, il compare les types de données lors de l'appel d'une méthode, de telle sorte que le fractionnement d'une grosse méthode individuelle en plusieurs sous-méthodes peut ralentir le code.

Cependant, nous vous recommandons vivement de ne pas optimiser prématurément, sous prétexte de peurs obscures. La vaste majorité des règles (par ex. : attribution de noms, commentaires) n'a pas le moindre impact négatif. Essayez de créer de façon propre, selon une approche orientée objet. Si quelque chose est trop lent, effectuez une mesure des performances. Après quoi seulement, vous pourrez prendre la décision, fondée sur les faits, de rejeter les règles en question.

Pour aller plus loin, lisez ces considérations extraites en partie du Chapitre 2 du livre de [Martin Fowler_ Refactoring_](https://martinfowler.com/books/refactoring.html) :

Dans une application classique, la majorité de la durée d'exécution concerne une infime proportion du code. 90 % de la durée d'exécution peuvent se rapporter à seulement 10 % du code. De plus, dans ABAP en particulier, il est probable qu'une vaste proportion de la durée d'exécution corresponde en réalité à du temps de base de données.

Ainsi, en termes d'utilisation des ressources, l'idéal n'est pas de s'efforcer de rendre _l'intégralité_ du code super efficace tout le temps. Nous ne vous suggérons pas d'ignorer les performances, mais plutôt de vous concentrer sur un code plus propre et mieux structuré lors du développement initial, et d'utiliser ABAP Profiler pour identifier les points critiques à optimiser.

En fait, il y a lieu d'ajouter qu'une telle approche aura un effet positif net sur les performances, car l'effort d'optimisation sera plus ciblé. Par ailleurs, il sera plus facile d'identifier les goulots d'étranglement au niveau des performances, ainsi que plus facile de refactoriser et ajuster du code bien structuré.

### Préférez la programmation orientée objet à la programmation procédurale

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Langage](#langage) > [Cette section](#préférez-la-programmation-orientée-objet-à-la-programmation-procédurale)

Les programmes orientés objet (classes, interfaces) étant mieux segmentés, ils peuvent être refactorisés et testés plus facilement que le code procédural (fonctions, programmes). Dans certaines situations néanmoins, vous devez indiquer des objets procéduraux (une fonction pour un RFC, un programme pour une transaction), mais ces objets doivent servir tout au plus à appeler une classe correspondante qui fournit la fonctionnalité réelle :

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> [Groupe de fonctions contre classes de fonctions](sub-sections/FunctionGroupsVsClasses.md) > décrit en détail les différences.

### Préférez des constructs de langage fonctionnel plutôt que procédural

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Langage](#langage) > [Cette section](#préférez-des-constructs-de-langage-fonctionnel-plutôt-que-procédural)

Ils sont généralement plus courts et plus naturels pour les développeurs modernes.

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

Parmi les règles détaillées ci-dessous, nombreuses sont simplement des rappels plus spécifiques de ce conseil général.

### Évitez les éléments de langage obsolètes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Langage](#langage) > [Cette section](#évitez-les-éléments-de-langage-obsolètes)

Lorsque vous mettez à niveau votre version ABAP, assurez-vous de l'absence d'élément de langage obsolète et abstenez-vous d'en utiliser.

Par exemple, les variables "hôte" placées dans une séquence d'échappement avec `@` dans l'instruction suivante clarifie un peu mieux ce qui correspond à une variable de programme et ce qui correspond à une colonne dans la base de données

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

par rapport à la [forme obsolète sans séquence d'échappement](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm)

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

Les dernières alternatives ont tendance à améliorer la lisibilité du code et à réduire les conflits de conception grâce à des paradigmes de programmation modernes. Basculer vers ces nouveautés peut vous permettre d'obtenir un code plus propre automatiquement.

Bien qu'ils fonctionnent encore, les éléments obsolètes peuvent ne plus bénéficier d'optimisations en matière de vitesse de traitement et d'utilisation de la mémoire.

Avec des éléments de langage modernes, vous pouvez plus facilement intégrer des jeunes développeurs ABAP, qui peut-être ne connaîtront pas les constructs obsolètes, parce qu'ils n'ont pas suivi des formations SAP comme c'était le cas auparavant.

La documentation SAP NetWeaver comporte une section stable qui répertorie les éléments de langage obsolètes, par exemple [NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm),[ NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm),[ NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm)et [NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm).

### Utilisez les modèles de conception de manière avisée

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Langage](#langage) > [Cette section](#utilisez-les-modèles-de-conception-de-manière-avisée)

Utilisez des modèles de conception lorsqu'ils sont appropriés et fournissent un avantage apparent. N'en utilisez pas partout, juste pour le plaisir.

## Constantes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#constantes)

### Utilisez des constantes et non des nombres magiques

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constantes](#constantes) > [Cette section](#utilisez-des-constantes-et-non-des-nombres-magiques)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

est plus clair que

```ABAP
" anti-pattern
IF abap_type = 'D'.
```

> Pour en savoir plus, lisez _Chapitre 17 : Indicateurs et heuristiques : G25 : > remplacer le nombre magiques par des constantes nommées_ dans le livre de [Robert C. Martin _Coder proprement_].

### Préférez les classes d'énumération aux interfaces de constantes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constantes](#constantes) > [Cette section](#préférez-les-classes-dénumération-aux-interfaces-de-constantes)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

ou

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

au lieu de mélanger des éléments non liés comme dans

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

> [Énumérations](sub-sections/Enumerations.md) > décrit les modèles d'énumération courants> , en traitant de leurs avantages et inconvénients.
> 
> Pour en savoir plus, lisez _Chapitre 17 : Indicateurs et heuristiques : J3 : constantes contre énumérations_ dans le livre de [Robert C. Martin _Coder proprement_].

### Si vous n'utilisez pas de classes d'énumération, regroupez vos constantes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constantes](#constantes) > [Cette section](#si-vous-nutilisez-pas-de-classes-dénumération-regroupez-vos-constantes)

Si vous recueillez des constantes de façon désordonnée, par exemple dans une interface, regroupez-les :

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

clarifie la relation par rapport :

```ABAP
" Anti-pattern
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

Le groupe vous fait bénéficier également d'un accès par groupe, pour la validation des entrées par exemple :

```ABAP
DO number_of_constants TIMES.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(<constant>).
  IF <constant> = input.
    is_valid = abap_true.
    RETURN.
  ENDIF.
ENDWHILE.
```

> Pour en savoir plus, lisez _Chapitre 17 : Indicateurs et heuristiques : G27 : privilégier la structure à une convention_ dans le livre de [Robert C. Martin _Coder proprement_].

## Variables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#variables)

### Préférez les déclarations en ligne aux déclarations initiales

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Variables](#variables) > [Cette section](#préférez-les-déclarations-en-ligne-aux-déclarations-initiales)

Si vous suivez ces directives, vos méthodes deviendront si courtes (3 à 5 instructions) qu'il vous paraîtra plus naturel d'utiliser des déclarations de variables en ligne à la première occurrence

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

plutôt que des déclarations de variables avec une section `DATA` distincte au début de la méthode

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

> Pour en savoir plus, lisez _Chapitre 5 : Mise en forme : distance verticale : déclarations de variables_ dans le livre de [Robert C. Martin _Coder proprement_].

### N'effectuez pas de déclarations en ligne dans les branchements facultatifs

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Variables](#variables) > [Cette section](#neffectuez-pas-de-déclarations-en-ligne-dans-les-branchements-facultatifs)

```ABAP
" anti-pattern
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

Cela fonctionne sans problème, car ABAP traite les déclarations en ligne comme si elles se trouvaient au début de la méthode. Cependant, cette façon de faire est extrêmement déroutante pour les lecteurs, en particulier si la méthode est longue et qu'il est difficile de repérer la déclaration au premier coup d'œil. Dans ce cas, convertissez votre déclaration en ligne en déclaration en déclaration initiale :

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> Pour en savoir plus, lisez _Chapitre 5 : Mise en forme : distance verticale : déclarations de variables_ dans le livre de [Robert C. Martin _Coder proprement_].

### Ne créez pas des chaînes de déclarations initiales

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Variables](#variables) > [Cette section](#ne-créez-pas-des-chaînes-de-déclarations-initiales)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /dirty/reader.
```

Le chaînage suggère que les variables définies sont liées à un niveau logique. Pour une utilisation cohérente, il faudrait veiller à ce que toutes les variables mises en chaîne soient effectivement liées entre elles, puis introduire d'autres groupes de chaînes pour ajouter des variables. Cela est possible, mais généralement, le jeu n'en vaut pas la chandelle.

En outre, le chaînage complique inutilement l'ajustement de la mise en forme et le refactoring, car chaque ligne apparaît différente et leur modification requiert donc de jouer avec les deux-points, les points et les virgules, qui n'en valent pas la peine.

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO /dirty/reader.
```

> Consultez également [N'alignez pas les clauses type](#nalignez-pas-les-clauses-type)
> En cas d'utilisation du chaînage de déclaration de données, utilisez alors une chaîne par groupe de variables liées entre elles.

### Préférez REF TO à FIELD-SYMBOL

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Variables](#variables) > [Cette section](#préférez-ref-to-à-field-symbol)

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

au lieu de son équivalent

```ABAP
" anti-pattern
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

sauf lorsque vous avez besoin de symboles de zone

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

Comme le démontrent les revues de code, les collaborateurs ont tendance à choisir l'un ou l'autre arbitrairement, "juste comme ça", "parce que nous créons toujours des boucles comme ça" ou "sans raison particulière". En raison de ces choix arbitraires, le lecteur perd du temps à se demander, pour rien, pourquoi l'un est utilisé plus que l'autre. Il faut donc remplacer ces choix arbitraires par des décisions fondées et précises. Notre recommandation est basée sur ce raisonnement :

- Les symboles de zone offrent des possibilités que les références n'offrent pas, telles que l'accès dynamique aux composants d'une structure. De la même manière, les références offrent des possibilités que les symboles de zone n'offrent pas, telles que la création d'une structure de données en typage dynamique. En résumé, il est impossible d'arrêter son choix sur une solution.

- Dans ABAP orienté objet, les références sont omniprésentes et inévitables, puisque tout objet est un `REF TO <class-name>`. A contrario, les symboles de zone sont strictement requis uniquement dans quelques cas spécifiques où intervient le typage dynamique. Les références sont ainsi naturellement privilégiées dans les programmes orientés objet.

- Les symboles de zone sont plus courts que les références, mais l'économie de mémoire qui en résulte est si infime qu'il peut assurément être négligé. De la même façon, la vitesse n'est pas un problème. Par conséquent, aucune raison propre aux performances ne nous invite à privilégier l'un ou l'autre.

> Pour en savoir plus, lisez l'article > [_Accès dynamique aux objets de données_ dans les Directives de programmation ABAP ](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm).

## Tables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#tables)

### Utilisez le bon type de table

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#utilisez-le-bon-type-de-table)

- En général, vous utilisez des tables `HASHED` pour les **grandes tables** qui sont **renseignées en une seule étape**, ne sont **jamais modifiées** et sont **souvent lues grâce à leur clé**. De par la consommation de mémoire et les données inutiles de traitement qui leur sont inhérentes, les tables de hachage se justifient uniquement pour les gros volumes de données et les nombreux accès en lecture. Chaque modification apportée au contenu des tables implique un recalcul coûteux du hachage, donc n'utilisez pas cette fonction pour les tables régulièrement modifiées.

- En général, vous utilisez des tables `SORTED` pour les **grandes tables** qui doivent être **triées en permanence**, sont **renseignées pas à pas** ou **doivent être modifiées**, sont **souvent lues grâce à une ou plusieurs clés complètes** ou traitées **dans un ordre donné**. L'ajout, la modification ou la suppression du contenu nécessite de trouver le bon emplacement d'insertion, mais n'implique pas d'ajuster le reste de l'index des tables. Les tables triées sont utiles uniquement en présence d'un grand nombre d'accès en lecture.

- Utilisez les tables `STANDARD` pour les **petites tables**, lorsque l'indexation produit plus de données inutiles que d'avantages, et pour les **"tableaux"**, lorsque vous ne vous souciez pas du tout de l'ordre des lignes ou vous souhaitez traiter celles-ci en suivant précisément l'ordre dans lequel elles ont été ajoutées. Même chose si différents accès à la table sont nécessaires (par ex. : accès indexé et accès trié via `SORT` et `BINARY SEARCH`.

> Il s'agit là des directives générales. > Pour aller plus loin, lisez l'article [_Sélection de catégorie de table_ dans l'Aide au langage ABAP ](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm).

### Évitez DEFAULT KEY

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#évitez-default-key)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

Souvent, DEFAULT KEY est ajouté uniquement pour que les dernières instructions fonctionnelles soient opérationnelles. À vrai dire, les clés en soi sont souvent superflues et gaspillent des ressources pour rien. Elles peuvent même conduire à des erreurs obscures, car elles ignorent les types de données numériques. Les instructions `SORT` et `DELETE ADJACENT` sans liste de zones explicite recourront à la clé primaire de la table interne qui, en cas d'utilisation de `DEFAULT KEY`, peut donner des résultats très inattendus si vous avez par exemple des zones numériques comme composant de la clé, utilisées notamment en combinaison avec `READ TABLE ... BINARY`, etc.

Indiquez explicitement les composants de la clé

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

ou recourez à `EMPTY KEY` si vous n'avez pas du tout besoin de clé.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> Pour aller plus loin, consultez le [blog de Horst Keller sur les _Tables internes avec EMPTY KEY_ ](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)
> **Attention :** `SORT` sur les tables internes avec `EMPTY KEY` n'effectuera aucun tri, > mais des avertissements de syntaxe seront émis si l'absence de clé peut être déterminée statistiquement.

### Préférez INSERT INTO TABLE à APPEND TO

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#préférez-insert-into-table-à-append-to)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` fonctionne avec tous les types de table et clé, ce qui vous facilite la tâche de refactoring du type de table et des définitions de clés si vos exigences en matière de performances évoluent.

Utilisez `APPEND TO` uniquement si vous utilisez une table `STANDARD` un peu comme un tableau, dans l'ambition d'insister sur le fait que l'entrée ajoutée doit être la dernière ligne.

### Préférez LINE_EXISTS à READ TABLE ou LOOP AT

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#préférez-line_exists-à-read-table-ou-loop-at)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

permet d'exprimer votre intention de façon plus claire et plus courte que

```ABAP
" anti-pattern
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

ou même

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Préférez READ TABLE à LOOP AT

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#préférez-read-table-à-loop-at)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

permet d'exprimer votre intention de façon plus claire et plus courte que

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

ou même

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Préférez LOOP AT WHERE à un IF imbriqué

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#préférez-loop-at-where-à-un-if-imbriqué)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

permet d'exprimer votre intention de façon plus claire et plus courte que

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Évitez les lectures de table superflues

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Tables](#tables) > [Cette section](#évitez-les-lectures-de-table-superflues)

Si vous vous _attendez_ à la présence d'une ligne, lisez-la une fois et réagissez à l'exception

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

au lieu d'encombrer et de ralentir le flux de contrôle principal avec une double lecture

```ABAP
" anti-pattern
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
DATA(row) = my_table[ key = input ].
```

> En plus d'améliorer les performances, cette façon de faire constitue une variante spécifique à la consigne plus générale [Activez les cas d'utilisation correcte ou la gestion des erreurs, mais pas les deux](#activez-les-cas-dutilisation-correcte-ou-la-gestion-des-erreurs-mais-pas-les-deux).

## Chaînes de caractères

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#chaînes-de-caractères)

### Utilisez ` pour définir des littéraux

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Chaînes de caractères](#chaînes-de-caractères) > [Cette section](#utilisez--pour-définir-des-littéraux)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

Évitez d'utiliser `'`, car cela ajoute une conversion de type superflue et le lecteur ne sait plus s'il gère un `CHAR` ou `STRING` :

```ABAP
" anti-pattern
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` passe en général, mais ne peut pas être utilisé pour `CONSTANTS` et ajoute des données inutiles lors de la définition d'une constante :

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### Utilisez | pour assembler du texte

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Chaînes de caractères](#chaînes-de-caractères) > [Cette section](#utilisez-|-pour-assembler-du-texte)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

Les modèles de chaîne de caractères mettent davantage en évidence ce qui correspond à des littéraux et ce qui correspond à des variables, en particulier si vous incorporez plusieurs variables dans un texte.

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Booléens

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#booléens)

### Utilisez les booléens de manière avisée

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Booléens](#booléens) > [Cette section](#utilisez-les-booléens-de-manière-avisée)

Dans bien des cas, il paraît naturel d'utiliser des booléens

```ABAP
" anti-pattern
is_archived = abap_true.
```

jusqu'à ce qu'un changement de perspective laisse à penser qu'une énumération aurait été un choix plus judicieux

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

En règle générale, il est déconseillé d'utiliser des booléens pour faire la distinction entre des types d'éléments, car presque toujours, vous tomberez sur des cas qui ne sont pas exclusivement l'un ou l'autre

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

[Fractionnez la méthode au lieu d'utiliser un paramètre d'entrée booléen](#fractionnez-la-méthode-au-lieu-dutiliser-un-paramètre-dentrée-booléen) explique également pourquoi vous devez toujours remettre en question les paramètres booléens.

> Pour en savoir plus, lisez [1](http://www.beyondcode.org/articles/booleanVariables.html)

### Utilisez ABAP_BOOL pour les booléens

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Booléens](#booléens) > [Cette section](#utilisez-abap_bool-pour-les-booléens)

```ABAP
DATA has_entries TYPE abap_bool.
```

N'utilisez pas le type générique `char1`. Bien qu'il soit compatible d'un point de vue technique, il masque le fait que nous traitons une variable booléenne.

Évitez également les autres types booléens, car ils entraînent souvent des effets secondaires étranges. Par exemple, `boolean` prend en charge une troisième valeur "undefined" (non définie) qui donnent lieu à de subtiles erreurs de programmation.

Dans certains cas, vous aurez peut-être besoin d'un élément du Dictionnaire ABAP, par exemple pour les zones de dynpro. `abap_bool` ne peut pas être utilisé à ce moment-là, car il est défini dans le groupe de types `abap`, et non dans le Dictionnaire ABAP. Dans ce cas, recourez à `boole_d` ou `xfeld`. Créez votre propre élément de données si vous avez besoin d'une description personnalisée.

> ABAP est peut-être l'unique langage de programmation qui n'est pas préconfiguré avec un type booléen universel. Cependant, il est impératif d'en avoir un. Cette recommandation est basée sur les Directives de programmation ABAP.

### Utilisez ABAP_TRUE et ABAP_FALSE pour les comparaisons

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Booléens](#booléens) > [Cette section](#utilisez-abap_true-et-abap_false-pour-les-comparaisons)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

N'utilisez pas les caractères équivalents `'X'` et `' '` ou `space` ; avec ceux-ci il est plus difficile d'identifier qu'il s'agit d'une expression booléenne :

```ABAP
" anti-pattern
has_entries = 'X'.
IF has_entries = space.
```

Évitez les comparaisons avec `INITIAL`. Cela oblige les lecteurs à se souvenir que la valeur par défaut de `abap_bool` est `abap_false` :

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> ABAP est peut-être l'unique langage de programmation qui n'est pas préconfiguré avec des "constantes" pour vrai et faux. Cependant, il est impératif d'en avoir. Cette recommandation est basée sur les Directives de programmation ABAP.

### Utilisez XSDBOOL pour définir des variables booléennes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Booléens](#booléens) > [Cette section](#utilisez-xsdbool-pour-définir-des-variables-booléennes)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

Le `IF`-`THEN`-`ELSE` équivalent est plus long et n'apporte rien de plus :

```ABAP
" anti-pattern
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool` est la meilleure méthode pour notre objectif car il produit directement un `char1` qui est le mieux adapté à notre type booléen `abap_bool`. Les fonctions équivalentes `boolc` et `boolx` produisent des types différents et nécessitent une conversion de type implicite superflue.

Nous convenons que le nom `xsdbool` est mal choisi et qu'il prête à confusion ; après tout, nous ne sommes pas du tout intéressés par les parties "définition de schéma XML" que le préfixe "xsd" suggère.

Une alternative possible pour `xsdbool` est la forme ternaire `COND`. Sa syntaxe est intuitive mais un peu plus longue, car elle répète inutilement le segment `THEN abap_true`, et elle nécessite de connaître la valeur par défaut implicite `abap_false`. C'est pourquoi nous la recommandons seulement comme solution secondaire.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## Conditions

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#conditions)

### Essayez de rendre les conditions positives

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Conditions](#conditions) > [Cette section](#essayez-de-rendre-les-conditions-positives)

```ABAP
IF has_entries = abap_true.
```

À titre de comparaison, regardez comme il est difficile de comprendre la même instruction quand elle est inversée :

```ABAP
" anti-pattern
IF has_no_entries = abap_false.
```

Le terme "essayez" du titre de la section signifie que vous ne devez pas pousser la contrainte jusqu'à vous retrouver avec quelque chose comme des [branchements IF vides](#aucun-branchement-if-vide) :

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> Pour en savoir plus, lisez _Chapitre 17 : Indicateurs et heuristiques : G29 : éviter les expressions conditionnelles négatives_ dans le livre de [Robert C. Martin _Coder proprement_].

### Préférez IS NOT à NOT IS

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Conditions](#conditions) > [Cette section](#préférez-is-not-à-not-is)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

La négation est logiquement équivalente, mais elle requiert une "inversion mentale" qui la rend plus difficile à comprendre.

```ABAP
" anti-pattern
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> Une variante plus spécifique par rapport à [Essayez de rendre les conditions positives](#essayez-de-rendre-les-conditions-positives). Également décrit dans la section [Constructs de langage alternatifs](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenalternative_langu_guidl.htm) dans les Directives de programmation ABAP.

### Efforcez-vous de décomposer les conditions complexes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Conditions](#conditions) > [Cette section](#efforcez-vous-de-décomposer-les-conditions-complexes)

Les conditions peuvent devenir plus simples lorsqu'on les décompose en parties élémentaires :

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

Plutôt que de laisser tout en place :

```ABAP
" anti-pattern
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> Utilisez les solutions rapides d'ABAP Development Tools pour extraire rapidement les conditions et créer les variables comme indiqué ci-dessus.

### Efforcez-vous d'extraire les conditions complexes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Conditions](#conditions) > [Cette section](#efforcez-vous-dextraire-les-conditions-complexes)

C'est presque toujours une bonne idée d'extraire des conditions complexes vers des méthodes dédiées :

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

## If

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#if)

### Aucun branchement IF vide

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [If](#if) > [Cette section](#aucun-branchement-if-vide)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

est plus court et plus clair que

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### Préférez CASE à ELSE IF pour des conditions alternatives multiples

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [If](#if) > [Cette section](#préférez-case-à-else-if-pour-des-conditions-alternatives-multiples)

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

`CASE` facilite la lecture d'un ensemble d'alternatives qui s'excluent mutuellement. Case peut-être plus rapide qu'une série de `IF`, car il peut convertir vers une commande microprocesseur différente au lieu d'une série de conditions évaluées l'une après l'autre. Vous pouvez insérer des nouveau cas rapidement sans avoir à répéter encore et encore la variable discernante. L'instruction permet même d'éviter d'éventuelles erreurs d'imbrication accidentelle des `IF`-`ELSEIF`.

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

### Gardez un niveau d'imbrication bas

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [If](#if) > [Cette section](#gardez-un-niveau-dimbrication-bas)

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

Les `IF` imbriqués deviennent rapidement difficiles à comprendre et exigent une quantité exponentielle de scénarios de test pour une couverture complète du code.

Les arbres de décision peuvent généralement être mis à part en créant des sous-méthodes et en déclarant des variables auxiliaires booléennes.

D'autres cas peuvent être simplifiés en fusionnant des IF, comme

```ABAP
IF <this> AND <that>.
```

au lieu de l'imbrication inutile

```ABAP
" anti-pattern
IF <this>.
  IF <that>.
```

## Expressions régulières

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#expressions-régulières)

### Préférez des méthodes plus simples aux expressions régulières

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Expressions régulières](#expressions-régulières) > [Cette section](#préférez-des-méthodes-plus-simples-aux-expressions-régulières)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

Les expressions régulières deviennent très vite difficiles à comprendre. Les cas simples sont généralement plus simples sans elles.

Habituellement les expressions régulières consomment plus de mémoire et de temps de traitement, parce qu'elles doivent être analysées dans un arbre d'expressions et compilées en un analyseur (matcher) exécutable lors de l'exécution. Des solutions simples peuvent faire le travail avec une boucle directe et une variable temporaire.

### Préférez les vérifications de base aux expressions régulières

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Expressions régulières](#expressions-régulières) > [Cette section](#préférez-les-vérifications-de-base-aux-expressions-régulières)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

au lieu de réinventer les choses

```ABAP
" anti-pattern
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> Il semble régner une tendance naturelle à fermer les yeux sur le principe "Ne vous répétez pas quand il y a des expressions régulières", voir section _Chapitre 17 : Indicateurs et heuristiques : Généralités : G5 : redondance_ dans le livre de [Robert C. Martin_ Coder proprement_].

### Efforcez-vous d'assembler des expressions régulières complexes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Expressions régulières](#expressions-régulières) > [Cette section](#efforcez-vous-dassembler-des-expressions-régulières-complexes)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

Certaines expressions régulières complexes deviennent plus simples lorsque vous montrez au lecteur comment elles sont construites à partir de parties plus élémentaires.

## Classes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#constructeurs)

### Classes : orientation objet

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Cette section](#classes--orientation-objet)

#### Préférez les objets aux classes statiques

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Classes : orientation objet](#classes-:-orientation-objet) > [Cette section](#préférez-les-objets-aux-classes-statiques)

Les classes statiques abandonnent tous les avantages gagnés initialement par l'orientation objet. En particulier, elles rendent quasi-impossible le remplacement des dépendances productives par des simulations de test dans les tests de module.

Si vous vous demandez s'il faut rendre une classe ou une méthode statique, la réponse sera presque toujours : non.

Une exception reconnue à cette règle : les classes utils de type brut. Leurs méthodes facilitent l'interaction avec certains types ABAP. Non seulement elles sont complètement sans statut, mais elles sont aussi tellement basiques qu'elles ressemblent à des instructions ABAP ou à des fonctions prédéfinies. Le facteur discriminant est que leurs consommateurs les lient à leur code si étroitement qu'en réalité, ils ne veulent pas les simuler dans les tests de module.

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

#### Préférez la composition à l'héritage

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Classes : orientation objet](#classes-:-orientation-objet) > [Cette section](#préférez-la-composition-à-lhéritage)

Évitez de créer des hiérarchies de classes avec un héritage. Favorisez plutôt la composition.

Un héritage propre est difficile à concevoir, car vous devez respecter des règles telles que le [principe de substitution de Liskov](https://en.wikipedia.org/wiki/Liskov_substitution_principle). Il est aussi difficile à comprendre, car il faut prendre conscience des principes directeurs qui sous-tendent la hiérarchie et les intégrer. L'héritage limite la réutilisation, parce que les méthodes tendent à être mises à disposition seulement pour des sous-classes. Il complique également le refactoring, car déplacer ou modifier des membres a tendance à nécessiter des modifications sur toute l'arborescence.

"Composition" signifie que vous concevez des petits objets indépendants, chacun servant un objectif précis. Ces objets peuvent être recombinés en objets plus complexes par des modèles de délégation et de façade simples. La composition peut produire un plus grand nombre de classes, mais c'est son seul inconvénient.

Ne laissez pas cette règle vous décourager d'utiliser de l'héritage quand c'est justifié. De bonnes applications existent pour l'héritage, par exemple le [modèle de conception composite](https://en.wikipedia.org/wiki/Composite_pattern). Demandez-vous simplement et objectivement si dans votre cas l'héritage va vraiment apporter plus d'avantages que d'inconvénients. En cas de doute, la composition est généralement le choix le plus sûr.

> [Interfaces vs classes abstraites](sub-sections/InterfacesVsAbstractClasses.md) compare certains détails.

#### Ne mélangez pas des paradigmes avec statut et sans statut dans la même classe

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Classes : orientation objet](#classes-:-orientation-objet)

Ne mélangez pas des paradigmes de programmation avec statut et sans statut dans la même classe.

Dans la programmation sans statut, les méthodes accèdent à des entrées et produisent des sorties, _sans aucun effet secondaire_, ce qui fait que les méthodes produisent le même résultat quel que soit le moment et l'ordre dans lequel elles sont appelées.

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

Dans la programmation avec statut, nous manipulons le statut interne des objets au travers de leurs méthodes, ce qui signifie qu'elle est _pleine d'effets secondaires_.

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

Les deux paradigmes sont corrects et ont leurs domaines d'application. Cependant, _mélanger_ les deux dans le même objet rend le code difficile à comprendre et voué à l'échec, avec d'obscures erreurs de transfert et des problèmes de synchronicité. Ne faites pas ça.

### Portée

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Cette section](#portée)

#### Globale par défaut, locale uniquement le cas échéant

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Portée](#portée) > [Cette section](#globale-par-défaut-locale-uniquement-le-cas-échéant)

Par défaut, travaillez avec des classes globales. Utilisez des classes locales seulement là où c'est justifié.

> Les classes globales sont celles qui sont visibles dans le Dictionnaire ABAP. Les classes locales vivent au sein d'un include d'un autre objet de développement et sont visibles uniquement pour cet autre objet.

Les classes locales sont adaptées

- pour des structures de données privées très spécifiques, par exemple un itérateur pour les données de la classe globale, qui sera toujours utilisé là seulement ;

- pour extraire un algorithme de pièces privées complexe, par exemple pour séparer cet algorithme de tri-agrégat multi-méthodes ciblant un objectif particulier du reste du code de votre classe ;

- pour permettre de simuler certains aspects de la classe globale, par exemple en extrayant tout accès à la base de données dans une classe locale distincte qui peut être remplacée par une simulation test dans les tests de module.

Les classes locales empêchent la réutilisation, parce qu'elles ne peuvent pas être utilisées ailleurs. Bien qu'elles soient aisées à extraire, les collaborateurs échouent même à les trouver, ce qui entraîne une double saisie de code indésirable. L'orientation, la navigation et le débogage dans des très longs includes de classes locales sont fastidieux et rébarbatifs. Comme ABAP bloque au niveau include, les collaborateurs ne pourront pas travailler simultanément sur différentes parties de l'include local (ce qui serait possible s'il s'agissait de classes globales distinctes).

Repensez votre utilisation des classes locales si

- votre include local s'étend sur des dizaines de classes et des milliers de lignes de code,
- vous voyez les classes globales comme des "packages" qui contiennent d'autres classes,
- vos classes globales finissent en coquilles vides,
- vous trouvez du code redondant répété dans plusieurs includes locaux,
- vos développeurs commencent à se bloquer les uns les autres et n'arrivent plus à travailler en parallèle,
- vos estimations de consignation atteignent des sommets, car vos équipes n'arrivent pas à comprendre leurs sous-arborescences locales respectives.

#### FINAL si non conçue pour l'héritage

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Portée](#portée) > [Cette section](#final-si-non-conçue-pour-lhéritage)

Définissez les classes qui ne sont pas explicitement conçues pour l'héritage sur `FINAL`.

Lorsque vous concevez une coopération entre classes, vous devez penser en premier lieu [composition, et non héritage](#préférez-la-composition-à-lhéritage). Activer l'héritage n'est pas une opération qui doit être effectuée à la légère, car cela implique de réfléchir à l'accès (`PROTECTED` ou `PRIVATE`) ainsi qu'au [principe de substitution de Liskov](https://en.wikipedia.org/wiki/Liskov_substitution_principle), et un grand nombre de parties internes de conception seront gelés. Si vous ne tenez pas compte de ces critères lors de votre conception de classe, vous devez alors empêcher l'héritage accidentel en définissant votre classe comme `FINAL`.

De bonnes applications _existent_ pour l'héritage, par exemple le [modèle de conception composite](https://en.wikipedia.org/wiki/Composite_pattern). Les Business Add-Ins peuvent également s'avérer plus utiles en autorisant les sous-classes, ce qui permet au client de réutiliser la plupart du code d'origine. Notez cependant que pour tous ces cas, l'héritage est configuré dès le but, à la conception.

Les classes incorrectes qui [n'implémentent pas les interfaces](#les-méthodes-dinstance-publiques-doivent-faire-partie-dune-interface) doivent être laissées comme non-`FINAL`, pour permettre aux consommateurs de les simuler dans leurs tests de module.

#### Membres PRIVATE par défaut, PROTECTED seulement si besoin

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Portée](#portée) > [Cette section](#membres-private-par-défaut-protected-seulement-si-besoin)

Définissez les attributs, méthodes et autres membres de classe sur `PRIVATE` par défaut.

Définissez-les sur `PROTECTED` uniquement si vous souhaitez activer les sous-classes qui les remplacent.

Les parties internes de classes doivent être mis à la disposition d'autrui uniquement au besoin. Sont inclus parmi ces parties internes non seulement les programmes appelant externes, mais aussi les sous-classes. Ne mettez pas trop d'informations à disposition, car cela peut causer des erreurs subtiles dues à des redéfinitions inattendues et entraver le refactoring, car les programmes externes gèlent directement les membres qui doivent pourtant demeurer "fluides".

#### Efforcez-vous d'utiliser un immuable au lieu d'un getter

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Portée](#portée) > [Cette section](#efforcez-vous-dutiliser-un-immuable-au-lieu-dun-getter)

Un immuable est un objet qui ne change jamais après sa création. Pour ce type d'objet, utilisez des attributs publics en lecture seule, plutôt que des méthodes getter.

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

au lieu de

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

> **Attention** : pour les objets qui eux **changent** de valeurs, n'utilisez pas des attributs publics en lecture seule. Autrement, ces attributs devront sans cesse être mis à jour, que leur valeur soit requise par un autre code ou non.

#### Utilisez READ-ONLY avec parcimonie

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Portée](#portée) > [Cette section](#utilisez-read-only-avec-parcimonie)

Nombre de langages de programmation modernes, en particulier Java, recommandent de définir les membres de classe en lecture seule lorsqu'il y a lieu, afin d'éviter les effets secondaires accidentels.

ABAP _offre_ bien l'option `READ-ONLY` pour les déclarations de données, mais nous vous recommandons de l'utiliser avec parcimonie.

Premièrement, l'option n'est disponible que dans `PUBLIC SECTION`, ce qui limite considérablement son champ d'application. Vous ne pouvez l'ajouter ni aux membres protégés ou privés, ni aux variables locales dans une méthode.

Deuxièmement, le fonctionnement de cette option est légèrement différent de ce à quoi les collaborateurs ont été habitués par les autres langages de programmation : les données en READ-ONLY peuvent tout de même être modifiées librement à partir de n'importe quelle méthode dans la classe elle-même, ses amis et ses sous-classes. Ce comportement est en contradiction avec celui le plus répandu dans les autres langages, où l'élément, une fois écrit, n'est plus jamais modifié. Cette différence peut conduire à de mauvaises surprises.

> Pour éviter tout malentendu : nous rappelons que protéger les variables contre la modification accidentelle est une bonne pratique. Nous vous recommanderions d'appliquer cette pratique à ABAP également s'il existait une instruction appropriée.

### Constructeurs

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Classes](#constructeurs) > [Cette section](#constructeurs)

#### Préférez NEW à CREATE OBJECT

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constructeurs](#constructeurs) > [Portée](#constructeurs) > [Cette section](#préférez-new-à-create-object)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

sauf si vous avez besoin de types dynamiques, bien sûr

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### Si votre classe globale est CREATE PRIVATE, laissez CONSTRUCTOR public

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constructeurs](#constructeurs) > [Portée](#constructeurs) > [Cette section](#si-votre-classe-globale-est-create-private-laissez-constructor-public)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

Nous reconnaissons que cette règle est contradictoire. Cependant, selon l'article [_Constructeur d'instance_ dans l'Aide ABAP](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm), il est nécessaire de définir `CONSTRUCTOR` dans `PUBLIC SECTION` pour garantir la validation de syntaxe et compilation correctes.

Cette règle s'applique uniquement aux classes globales. Dans les classes locales, définissez le constructeur sur privé, comme le veut la logique.

#### Préférez les méthodes de création statiques multiples aux paramètres facultatifs

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constructeurs](#constructeurs) > [Portée](#constructeurs) > [Cette section](#prefer-multiple-static-factory-methods-to-optional-parameters)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP ne prend pas en charge la [surcharge](https://en.wikipedia.org/wiki/Function_overloading). Utilisez des variantes de noms, au lieu de paramètres facultatifs, pour réaliser la sémantique souhaitée.

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

La directive générale [_Fractionnez les méthodes au lieu d'ajouter des paramètres OPTIONAL_](#fractionnez-les-méthodes-au-lieu-dajouter-des-paramètres-optional) explique le raisonnement derrière.

Efforcez-vous de convertir les constructions complexes en une construction à étapes multiples avec le [modèle de conception Builder](https://en.wikipedia.org/wiki/Builder_pattern).

#### Utilisez des noms descriptifs pour les méthodes de création multiples

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constructeurs](#constructeurs) > [Portée](#constructeurs) > [Cette section](#utilisez-des-noms-descriptifs-pour-les-méthodes-de-création-multiples)

`new_`, `create_` et `construct_` sont des bons termes pour débuter les méthodes de création. Les collaborateurs les relient intuitivement à la construction des objets. Ils se combinent parfaitement aussi avec des groupes verbaux comme `new_from_template`, `create_as_copy` ou `create_by_name`.

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

plutôt que du code insignifiant comme

```ABAP
" anti-pattern
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### Codez des singletons uniquement là où les instances multiples n'ont pas de sens

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Constructeurs](#constructeurs) > [Portée](#constructeurs) > [Cette section](#codez-des-singletons-uniquement-là-où-les-instances-multiples-nont-pas-de-sens)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

Appliquez le modèle singleton lorsque votre conception orientée objet indique qu'il n'y a pas de sens à avoir une seconde instance pour tel ou tel élément. Utilisez-le pour veiller à ce que chaque consommateur travaille avec les mêmes outils, au même statut et avec les mêmes données.

N'utilisez pas le modèle singleton par habitude ou en considération des performances. Il s'agit du modèle le plus surutilisé et appliqué à mauvais escient. Il produit des effets croisés inattendus et complique inutilement les tests. En l'absence de motifs liés à la conception justifiant la définition d'un objet unitaire, laissez le consommateur prendre cette décision. Il pourra toujours obtenir le même résultat en recourant à des moyens externes au constructeur, par exemple, avec une factory.

## Méthodes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#méthodes)

Ces règles s'appliquent aux méthodes dans les classes et les modules fonctions.

### Appels

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#appels)

#### Préférez les appels fonctionnels aux appels procéduraux

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Appels](#appels) > [Cette section](#préférez-les-appels-fonctionnels-aux-appels-procéduraux)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Si le typage dynamique interdit les appels fonctionnels, recourez au style procédural

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Parmi les règles détaillées ci-dessous, nombreuses sont simplement des variantes plus spécifiques de ce conseil.

#### Omettez RECEIVING

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Appels](#appels) > [Cette section](#omettez-receiving)

```ABAP
DATA(sum) = aggregate_values( values ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### Omettez le mot-clé facultatif EXPORTING

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Appels](#appels) > [Cette section](#omettez-le-mot-clé-facultatif-exporting)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Omettez le nom du paramètre dans les appels de paramètre unique

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Appels](#appels) > [Cette section](#omettez-le-nom-du-paramètre-dans-les-appels-de-paramètre-unique)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates( list = list ).
```

Dans certains cas, cependant, le nom de la méthode seul n'est pas assez clair et il peut être utile de répéter le nom du paramètre pour faciliter la compréhension :

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### Omettez l'autoréférence me lorsque vous appelez une méthode d'instance

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Appels](#appels) > [Cette section](#omettez-lautoréférence-me-lorsque-vous-appelez-une-méthode-dinstance)

Comme l'autoréférence `me->` est implicitement définie par le système, omettez-la lorsque vous appelez une méthode d'instance

```ABAP
DATA(sum) = aggregate_values( values ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
DATA(sum) = me->aggregate_values( values ).
```

### Méthodes : orientation objet

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#méthodes--orientation-objet)

#### Préférez les méthodes d'instance aux méthodes statiques

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Méthodes : orientation objet](#méthodes--orientation-objet) > [Cette section](#préférez-les-méthodes-dinstance-aux-méthodes-statiques)

Les méthodes doivent être des membres d'instance par défaut. Les méthodes d'instance reflètent mieux la "condition de l'objet" de la classe. Elles peuvent être simulées plus facilement dans les tests de module.

```ABAP
METHODS publish.
```

Les méthodes doivent être statiques uniquement dans des cas exceptionnels, comme les méthodes de création statiques.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### Les méthodes d'instance publiques doivent faire partie d'une interface

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Méthodes : orientation objet](#méthodes-:-orientation-objet) > [Cette section](#les-méthodes-dinstance-publiques-doivent-faire-partie-dune-interface)

Les méthodes d'instance publiques doivent toujours faire partie d'une interface. Elles séparent les dépendances et simplifient leur simulation dans les tests de module.

```ABAP
METHOD /clean/blog_post~publish.
```

Dans une orientation objet propre, il y a peu d'intérêt à avoir une méthode publique sans interface (à quelques exceptions près, notamment les classes d'énumération, qui ne feront jamais l'objet d'une implémentation alternative et ne seront jamais simulées dans des scénarios de test.

> [Interfaces vs classes abstraites](sub-sections/InterfacesVsAbstractClasses.md) décrit la raison pour laquelle cette règle s'applique également aux classes qui écrasent les méthodes héritées.

### Nombre de paramètres

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#nombre-de-paramètres)

#### Visez un petit nombre de paramètres IMPORTING, dans l'idéal moins de trois

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Nombre de paramètres](#nombre-de-paramètres) > [Cette section](#visez-un-petit-nombre-de-paramètres-importing-dans-lidéal-moins-de-trois)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

est plus clair que

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

Les paramètres d'entrée, s'ils sont trop nombreux, font exploser la complexité d'une méthode, car elle doit alors traiter un nombre exponentiel de combinaisons. Un grand nombre de paramètres est un signe indiquant que la méthode accomplira certainement plusieurs choses.

Vous pouvez réduire le nombre de paramètres en les combinant en plusieurs ensembles significatifs comportant des structures et des objets.

#### Fractionnez les méthodes au lieu d'ajouter des paramètres OPTIONAL

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Nombre de paramètres](#nombre-de-paramètres) > [Cette section](#fractionnez-les-méthodes-au-lieu-dajouter-des-paramètres-optional)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

pour atteindre la sémantique souhaitée, puisque ABAP ne prend pas en charge la [surcharge](https://en.wikipedia.org/wiki/Function_overloading).

```ABAP
" anti-pattern
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

Les paramètres facultatifs déroutent les programmes appelant :

- Lesquels sont véritablement requis ?
- Quelles combinaisons sont valides ?
- Lesquels s'excluent mutuellement ?

Le recours à plusieurs méthodes avec des paramètres spécifiques pour les cas d'utilisation permet d'éviter cette confusion, en indiquant clairement les combinaisons de paramètres qui sont valides et attendus.

#### Utilisez PREFERRED PARAMETER avec parcimonie

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Nombre de paramètres](#nombre-de-paramètres) > [Cette section](#utilisez-preferred-parameter-avec-parcimonie)

Avec l'option `PREFERRED PARAMETER`, il est plus difficile de distinguer les paramètres qui sont véritablement renseignés, et encore plus difficile de comprendre le code. En minimisant le nombre de paramètres, en particulier ceux étant facultatifs, vous réduisez automatiquement le besoin d'utiliser `PREFERRED PARAMETER`.

#### Utilisez RETURN, EXPORT ou CHANGE pour un seul et unique paramètre

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Nombre de paramètres](#nombre-de-paramètres) > [Cette section](#utilisez-return-export-ou-change-pour-un-seul-et-unique-paramètre)

Une bonne méthode fait _une seule chose_. Ainsi, par extrapolation de cette règle, une méthode doit aussi renvoyer une seule chose. Si les paramètres de sortie de votre méthode ne forment _pas_ une entité logique, votre méthode accomplit plusieurs choses et doit être fractionnée.

Dans certains cas, la sortie est une entité logique qui comprend plusieurs choses. Il est plus facile de représenter ces cas en renvoyant une structure ou un objet :

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

au lieu de

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

Par rapport aux multiples paramètres EXPORTING notamment, cela permet aux collaborateurs d'utiliser un style d'appel fonctionnel, vous évite d'avoir à réfléchir à `IS SUPPLIED` et empêche les oublis accidentels quand il s'agit d'extraire les informations `ERROR_OCCURRED` vitales.

Au lieu d'utiliser de multiples paramètres de sortie facultatifs, fractionnez la méthode d'après des modèles d'appels significatifs :

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

### Types de paramètre

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#types-de-paramètre)

#### Préférez RETURNING à EXPORTING

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Types de paramètre](#types-de-paramètre) > [Cette section](#préférez-returning-à-exporting)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

au lieu de la version inutilement plus longue

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

`RETURNING` non seulement permet de raccourcir l'appel, mais aussi autorise le chaînage de méthodes et empêche les [erreurs où entrées et sorties sont identiques](#soyez-vigilant-si-lentrée-et-la-sortie-peuvent-être-identiques).

#### N'hésitez pas à utiliser RETURNING avec de grandes tables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Types de paramètre](#types-de-paramètre) > [Cette section](#nhésitez-pas-à-utiliser-returning-avec-de-grandes-tables)

Bien que la documentation relative au langage ABAP et les guides de performance disent le contraire, ils sont rares les cas où le transfert d'une grande table ou d'une table profondément imbriquée dans un paramètre VALUE entraîne _réellement_ des problèmes de performance. Par conséquent, nous vous recommandons d'utiliser en règle générale

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

Si vraiment vous obtenez preuve du contraire (c'est-à-dire, une mesure indiquant de mauvaises performances), vous pouvez recourir au style procédural plus fastidieux

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

> Cette section contredit les Directives de programmation ABAP et les contrôles Code Inspector, qui tous deux suggèrent que les grandes tables doivent être exportées par référence afin d'éviter des baisses de performance. À chaque tentative, nous ne sommes pas parvenus à reproduire ces baisses de performance et de mémoire, et avons reçu une notification à propos de l'optimisation du noyau, qui permet généralement d'améliorer la performance de RETURNING.

#### Utilisez soit RETURNING, soit EXPORTING, soit CHANGING, mais ne les utilisez pas en combinaison

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Types de paramètre](#types-de-paramètre) > [Cette section](#utilisez-soit-returning-soit-exporting-soit-changing-mais-ne-les-utilisez-pas-en-combinaison)

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

au lieu de mélanges déroutants comme

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

L'utilisation de différentes sortes de paramètres de sortie est un signe indiquant que la méthode accomplit plus d'une chose. Cela déroute le lecteur et complique inutilement l'appel de la méthode.

Les générateurs, qui utilisent leur entrée lorsqu'ils génèrent leur sortie, peuvent faire exception à cette règle :

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

Cependant, même ceux-là peuvent être plus clairs par conversion de l'entrée en objet :

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### Utilisez CHANGING avec parcimonie, lorsque cela est adapté

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Types de paramètre](#types-de-paramètre) > [Cette section](#utilisez-changing-avec-parcimonie-lorsque-cela-est-adapté)

`CHANGING` doit être réservé aux cas où une variable locale existante déjà renseignée est mise à jour seulement à quelques emplacements :

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

Ne forcez pas vos programmes appelant à introduire des variables locales superflues uniquement pour renseigner votre paramètre `CHANGING`. N'utilisez pas les paramètres `CHANGING` pour renseigner initialement une variable auparavant vide.

#### Fractionnez la méthode au lieu d'utiliser un paramètre d'entrée booléen

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Types de paramètre](#types-de-paramètre) > [Cette section](#fractionnez-la-méthode-au-lieu-dutiliser-un-paramètre-dentrée-booléen)

Souvent, la présence de paramètres d'entrée booléens est un signe indiquant qu'une méthode accomplit _deux_ choses, au lieu d'une.

```ABAP
" anti-pattern
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

En outre, les appels de méthode avec un paramètre booléen unique (et donc sans nom) ont tendance à rendre le sens du paramètre un peu plus obscur.

```ABAP
" anti-pattern
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

Fractionner la méthode peut aider à simplifier le code des méthodes et à mieux décrire les différentes intentions

```ABAP
update_without_saving( ).
update_and_save( ).
```

Le point de vue communément adopté suggère qu'il est possible d'utiliser des setters pour les variables booléens :

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> Pour en savoir plus, lisez [1](http://www.beyondcode.org/articles/booleanVariables.html) [2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/) [3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### Noms de paramètres

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#noms-de-paramètres)

#### Efforcez-vous d'appeler "RESULT" le paramètre RETURNING

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Noms de paramètres](#noms-de-paramètres) > [Cette section](#efforcez-vous-dappeler-result-le-paramètre-returning)

Généralement, si les noms de méthodes sont bien choisis, le paramètre `RETURNING` n'a même pas besoin d'avoir un nom. Ce dernier ne ferait qu'imiter le nom de la méthode ou répéter quelque chose d'évident.

La répétition d'un nom de membre peut même provoquer des conflits qui doivent être résolus par l'ajout d'un `me->` superflu.

```ABAP
" anti-pattern
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

Dans ces cas, appelez simplement le paramètre `RESULT`, ou bien quelque chose comme `RV_RESULT` si vous préférez la notation hongroise.

Nommez le paramètre `RETURNING` si ce qu'il désigne n'est _pas_ évident (par exemple, dans les méthodes qui renvoient `me` pour le chaînage de méthodes ou dans les méthodes qui créent quelque chose, mais qui, au lieu de renvoyer l'entité créée, renvoie uniquement sa clé ou autre.

### Initialisation de paramètres

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#initialisation-de-paramètres)

#### Réinitialisez ou écrasez les paramètres de référence EXPORTING

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Initialisation de paramètres](#initialisation-de-paramètres) > [Cette section](#réinitialisez-ou-écrasez-les-paramètres-de-référence-exporting)

Les paramètres de référence font référence aux zones de mémoire existantes qui peuvent être renseignées au préalable. Supprimez-les ou écrasez-les pour fournir des données fiables :

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

> Code Inspector et Checkman mettent l'accent que les variables `EXPORTING` qui ne sont jamais écrites. Utilisez ces contrôles statiques pour éviter cette source d'erreur assez obscure.

##### Soyez vigilant si l'entrée et la sortie peuvent être identiques

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Initialisation de paramètres](#initialisation-de-paramètres) > [Cette section](#soyez-vigilant-si-lentrée-et-la-sortie-peuvent-être-identiques)

En général, il est judicieux de supprimer le paramètre qui apparaît comme le premier élément dans la méthode, après les déclarations de type et de données. Cela permet de repérer plus facilement l'instruction et d'éviter que la valeur encore contenue soit accidentellement utilisée par des instructions ultérieures.

Cependant, certaines configurations de paramètres peuvent utiliser la même variable comme entrée et sortie. Dans ce cas, une utilisation préalable de `CLEAR` supprimera la valeur d'entrée avant son utilisation, ce qui donnera lieu à des résultats incorrects.

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

Efforcez-vous de restructurer ces méthodes en remplaçant `EXPORTING` par `RETURNING`. Efforcez-vous également d'écraser le paramètre `EXPORTING` dans une instruction de calcul à résultat unique. Si aucune de ces solutions ne fonctionne, recourez à un `CLEAR` ultérieurement.

#### Ne réinitialisez pas les paramètres VALUE

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Initialisation de paramètres](#initialisation-de-paramètres) > [Cette section](#ne-réinitialisez-pas-les-paramètres-value)

Les paramètres qui fonctionnent par `VALUE` sont transférés en tant que nouvelles zones de mémoire distinctes, vides par définition. Ne les supprimez pas à nouveau :

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

Les paramètres `RETURNING` sont toujours des paramètres `VALUE`, donc vous ne devez jamais les supprimer :

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### Corps de la méthode

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#corps-de-la-méthode)

#### Faites une chose, faites-la bien et ne faites que cela

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Corps de la méthode](#corps-de-la-méthode) > [Cette section](#faites-une-chose-faites-la-bien-et-ne-faites-que-cela)

Une méthode doit faire une seule et unique chose, de la meilleure manière possible.

Une méthode ne fait probablement qu'une chose si

- elle a [un petit nombre de paramètres d'entrée](#visez-un-petit-nombre-de-paramètres-importing-dans-lidéal-moins-de-trois) ;
- ses paramètres d'entrée [n'incluent pas des booléens](#fractionnez-la-méthode-au-lieu-dutiliser-un-paramètre-dentrée-booléen) ;
- elle n'a [qu'un seul paramètre de sortie](#utilisez-return-export-ou-change-pour-un-seul-et-unique-paramètre) ;
- elle est [petite](#privilégiez-les-méthodes-courtes) ;
- elle [descend d'un niveau d'abstraction](#descendez-dun-niveau-dabstraction) ;
- vous ne pouvez pas en extraire d'autres méthodes significatives ;
- vous ne pouvez pas regrouper de façon significative ses instructions en sections.

#### Activez les cas d'utilisation correcte ou la gestion des erreurs, mais pas les deux

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Corps de la méthode](#corps-de-la-méthode) > [Cette section](#activez-les-cas-dutilisation-correcte-ou-la-gestion-des-erreurs-mais-pas-les-deux)

Au-delà de la règle [_Faites une chose, faites-la bien et ne faites que cela_](#faites-une-chose-faites-la-bien-et-ne-faites-que-cela), plus spécifiquement, une méthode doit suivre les cas d'utilisation correcte pour lesquels elle a été créée ou, en cas d'impossibilité, suivre la voie détournée de la gestion des erreurs, mais certainement pas les deux.

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

peut être décomposé en

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

ou, pour souligner la partie propre à la validation

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

#### Descendez d'un niveau d'abstraction

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Corps de la méthode](#corps-de-la-méthode) > [Cette section](#descendez-dun-niveau-dabstraction)

Les instructions dans une méthode doivent se situer un niveau d'abstraction en-dessous de la méthode elle-même. Elles doivent toutes apparaître au même niveau d'abstraction.

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

au lieu des mélanges déroutants de concepts de bas niveau (`trim`, `to_upper`, ...) et haut niveau (`publish`, ...) comme

```ABAP
" anti-pattern
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

Voici une méthode fiable pour déterminer le bon niveau d'abstraction : laissez l'auteur de la méthode expliquer brièvement, en quelques mots, à quoi sert la méthode, sans même regarder le code. Les points qu'il/elle numérote correspondent aux sous-méthodes que la méthode devra appeler ou aux instructions qu'elle devra exécuter.

#### Privilégiez les méthodes courtes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Corps de la méthode](#corps-de-la-méthode) > [Cette section](#privilégiez-les-méthodes-courtes)

Les méthodes doivent comporter moins de 20 instructions, le nombre optimal se situant autour de 3 à 5 instructions.

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

La déclaration `DATA` suivante permet à elle seule de constater que la méthode supérieure fait bien plus qu'une seule chose :

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

Bien sûr, dans certaines circonstances, il n'est pas logique de réduire davantage une méthode assez vaste. Cela est tout à fait acceptable tant que la méthode reste [axée sur une chose](#faites-une-chose-faites-la-bien-et-ne-faites-que-cela) :

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

Cependant, il est toujours pertinent de vérifier si le code détaillé cache un modèle plus adapté :

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> Le découpage de méthodes pour les réduire au minimum peut avoir un effet négatif sur les performances, puisqu'il en résulte une augmentation du nombre d'appels de méthode. La section [_Tenez compte des performances_](#tenez-compte-des-performances) donne des conseils pour trouver l'équilibre entre un code propre et des performances optimales.

### Flux de contrôle

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Cette section](#flux-de-contrôle)

#### Échec accéléré

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Flux de contrôle](#flux-de-contrôle) > [Cette section](#échec-accéléré)

Procédez à des contrôles de validation et de réussite dès que possible :

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

Dans le cas de validations tardives, les problèmes sont plus difficiles à détecter et à comprendre. De plus, il est probable que de nombreuses ressources ont déjà été gaspillées pour en arriver là.

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

#### CHECK contre RETURN

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Flux de contrôle](#flux-de-contrôle) > [Cette section](#check-contre-return)

Il n'existe aucun consensus quant à l'utilisation de `CHECK` ou `RETURN` pour quitter une méthode si l'entrée ne répond pas aux attentes.

`CHECK` permet clairement de raccourcir la syntaxe

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

mais le nom de l'instruction ne révèle pas ce qui se passe en cas d'échec de la condition, de telle sorte que les collaborateurs comprendront probablement mieux la forme longue :

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD:
```

Vous pouvez éluder complètement la question, en inversant la validation et en adoptant un flux de contrôle à retour unique.

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD:
```

Dans tous les cas, demandez-vous si l'absence de résultat renvoyé est vraiment le comportement approprié. Les méthodes doivent fournir un résultat significatif, c'est-à-dire un paramètre de retour renseigné ou une exception. Dans nombre de cas, l'absence de résultat renvoyé équivaut au renvoi de `null`, ce qui doit être évité.

> La [section _Quitter les procédures_ dans les Directives de programmation ABAP ](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm) recommande d'utiliser `CHECK` dans cette instance. La communauté qui discute sur ce point estime que l'instruction n'est pas claire, à tel point que peu de collaborateurs réussiront à comprendre le comportement du programme.

#### Évitez d'utiliser CHECK dans d'autres positions

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Méthodes](#méthodes) > [Flux de contrôle](#flux-de-contrôle) > [Cette section](#évitez-dutiliser-check-dans-dautres-positions)

N'utilisez pas `CHECK` en dehors de la section d'initialisation d'une méthode. L'instruction suit un comportement différent suivant sa position et peut entraîner des effets obscurs et inattendus.

Par exemple, [`CHECK` dans une boucle (`LOOP`) termine l'itération actuelle et passe à la suivante](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm) ; les collaborateurs pourraient s'attendre accidentellement à ce que cela termine la méthode ou quitte la boucle.

> Source : [section _Quitter les procédures_ dans les Directives de programmation ABAP ](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm). Notez que cette consigne contredit la [référence sur les mots-clés pour `CHECK` dans les boucles ](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm).

## Gestion des erreurs

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#gestion-des-erreurs)

### Messages

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Cette section](#messages)

#### Facilitez la recherche des messages

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Messages](#messages) > [Cette section](#facilitez-la-recherche-des-messages)

Pour trouver plus facilement les messages via une recherche par cas d'utilisation avec la transaction SE91, utilisez le modèle suivant :

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

Si la variable `message` n'est pas nécessaire, ajoutez le pragma `##NEEDED` :

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

Évitez l'exemple suivant :

```ABAP
" anti-pattern
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

Il s'agit d'un contre-modèle, car :
- il contient un code inaccessible ;
- il teste l'égalité d'une condition qui ne peut jamais être vraie.

### Codes retour

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Cette section](#codes-retour)

#### Préférez les exceptions aux codes retour

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Codes retour](#codes-retour) > [Cette section](#préférez-les-exceptions-aux-codes-retour)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

au lieu de

```ABAP
" anti-pattern
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

Les exceptions présentent plusieurs avantages par rapport aux codes retour :

- Les exceptions maintiennent vos signatures de méthode propres : vous pouvez renvoyer le résultat d'une méthode comme paramètre `RETURNING` et lever des exceptions en parallèle. Les codes retour polluent vos signatures avec des paramètres supplémentaires pour la gestion des erreurs.

- Le programme appelant n'a pas besoin de répondre immédiatement aux exceptions. Il peut simplement s'en tenir aux cas d'utilisation correcte de son code. La gestion des exceptions `CATCH` peut se trouver à la toute fin de sa méthode ou carrément en dehors.

- Les exceptions peuvent fournir des détails sur l'erreur dans leurs attributs et par le biais des méthodes. Les codes retour vous obligent à imaginer une autre solution par vous-même (par exemple, renvoyer un journal également).

- L'environnement rappelle au programme appelant les erreurs de syntaxe pour la gestion des exceptions. Les codes retour peuvent être accidentellement ignorés sans que personne ne s'en aperçoive.

#### Ne laissez pas passer des erreurs

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Codes retour](#codes-retour) > [Cette section](#ne-laissez-pas-passer-des-erreurs)

Si vous devez utiliser des codes retour, par exemple parce que vous utilisez Fonctions et un code plus ancien n'étant pas sous votre contrôle, veillez à ne pas laisser passer des erreurs.

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

### Exceptions

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Cette section](#exceptions)

#### Les exceptions sont pour les erreurs, et non pour les cas normaux

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Exceptions](#exceptions) > [Cette section](#les-exceptions-sont-pour-les-erreurs-et-non-pour-les-cas-normaux)

```ABAP
" anti-pattern
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

Tout cas normal et valide doit être traité avec des paramètres de résultat normaux.

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

Vous devez réserver les exceptions aux cas qui sont inattendus et qui reflètent des situations d'erreur.

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

La mauvaise utilisation des exceptions induit le lecteur en erreur, celui-ci pouvant penser qu'il existe un problème alors que tout va parfaitement bien. Les exceptions prennent plus de temps que le code normal, car elles doivent être construites et rassemblent souvent de nombreuses informations contextuelles.

#### Utilisez des exceptions basées sur une classe

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Exceptions](#exceptions) > [Cette section](#utilisez-des-exceptions-basées-sur-une-classe)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

Les exceptions non basées sur une classe qui sont obsolètes présentent les mêmes fonctionnalités que les codes retour et ne doivent plus être utilisées.

```ABAP
" anti-pattern
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### Levée d'exceptions

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Cette section](#levée-dexceptions)

#### Utilisez vos propres surclasses

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#utilisez-vos-propres-surclasses)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```

Efforcez-vous de créer des surclasses abstraites pour chaque type d'exception pour votre application, plutôt que d'établir directement un sous-classement des Foundation Classes. Cela vous permet d'intercepter (`CATCH`) toutes _vos_ exceptions. Vous pouvez également ajouter des fonctionnalités communes à toutes les exceptions, telles que le traitement de texte spécial. `ABSTRACT` empêche les collaborateurs d'utiliser accidentellement des erreurs non descriptives directement.

#### Levez un seul type d'exception

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#levez-un-seul-type-dexception)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

Dans la grande majorité des cas, lever plusieurs types d'exception n'a aucune utilité. Généralement, le programme appelant n'a ni l'intérêt ni la capacité de faire la distinction des situations d'erreur. Par conséquent, il les traitera tous de la même manière en général, et dans ce cas, pourquoi prendre la peine de les distinguer au début ?

```ABAP
" anti-pattern
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

Pour identifier les différentes situations d'erreur, il existe une meilleure solution : utiliser un seul type d'exception, mais ajouter des sous-classes qui permettent (mais ne nécessitent pas) de réagir aux situations d'erreur individuelles, tel que décrit dans [Utilisez des surclasses pour permettre aux programmes appelant d'identifier les situations d'erreur](#utilisez-des-surclasses-pour-permettre-aux-programmes-appelant-didentifier-les-situations-derreur).

#### Utilisez des surclasses pour permettre aux programmes appelant d'identifier les situations d'erreur

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#utilisez-des-surclasses-pour-permettre-aux-programmes-appelant-didentifier-les-situations-derreur)

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

S'il existe une grande variété de situations d'erreur, utilisez plutôt des codes d'erreur :

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

#### Levez CX_STATIC_CHECK pour les exceptions gérables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#levez-cx_static_check-pour-les-exceptions-gérables)

Si une exception peut être anticipée et raisonnablement traitée par le programme récepteur, levez une exception contrôlée héritée de `CX_STATIC_CHECK` : échec de la validation des entrées utilisateur, ressource manquante pour laquelle il existe des procédures de secours, etc.

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

Ce type d'exception _doit_ être indiqué dans les signatures de méthode et _doit_ être intercepté ou transmis pour éviter les erreurs de syntaxe. Ainsi, le consommateur peut le voir facilement, ne sera pas surpris par une exception inattendue et prendra soin de réagir à la situation d'erreur.

> Cette règle est en accord avec les [Directives de programmation ABAP ](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm), mais en contradiction avec l'ouvrage de [Robert C. Martin _Coder proprement_], qui recommande de privilégier les exceptions contrôlées. Lisez [Exceptions](sub-sections/Exceptions.md) pour en comprendre les raisons.

#### Levez CX_NO_CHECK pour les situations généralement irrécupérables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#levez-cx_no_check-pour-les-situations-généralement-irrécupérables)

Si une exception est grave au point que le programme récepteur a peu de chances de s'en remettre, utilisez `CX_NO_CHECK` : échec de lecture d'une ressource obligatoire, échec de conversion de la dépendance demandée, etc.

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` _ne peut pas_ être déclaré dans les signatures de méthode, de telle sorte que sa présence sera considérée comme une mauvaise surprise par le consommateur. En cas de situations irrécupérables, vous pouvez utiliser cette exception, car le consommateur ne pourra rien en faire d'utile de toute façon.

Cependant, dans _certains_ cas, le consommateur souhaite en réalité identifier ce type d'échec et y réagir. Par exemple, un gestionnaire des dépendances peut lever une exception `CX_NO_CHECK` s'il est impossible de fournir une implémentation pour une interface demandée, car le code d'application normal ne pourra pas continuer. Cependant, il peut y avoir un programme de test qui tente d'instancier toutes sortes d'éléments, juste pour vérifier leur bon fonctionnement. Ce programme de test signalera l'erreur simplement sous la forme d'une entrée rouge dans une liste. Ce service devrait pouvoir intercepter et ignorer l'exception, plutôt que d'être forcé à effectuer un vidage de la mémoire.

#### Pensez à CX_DYNAMIC_CHECK pour les exceptions évitables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#pensez-à-cx_dynamic_check-pour-les-exceptions-évitables)

Les cas d'utilisation pour `CX_DYNAMIC_CHECK` sont rares et en général, nous vous recommandons de recourir aux autres types d'exception. Cependant, vous pouvez songer à utiliser ce type d'exception en remplacement de `CX_STATIC_CHECK` si le programme appelant dispose d'un contrôle conscient complet sur la survenue des erreurs.

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

Par exemple, pensez à la méthode `get_db_length_decs` de la classe `cl_abap_math`, qui vous indique le nombre de chiffres et de décimales d'un nombre décimal en virgule flottante. Cette méthode lève l'exception dynamique `cx_parameter_invalid_type` si le paramètre d'entrée ne reflète pas un nombre décimal en virgule flottante. Généralement, cette méthode sera appelée pour une variable typée de façon complète et statistique, de façon à ce que le développeur sache si cette exception risque ou non de survenir. Dans ce cas, l'exception dynamique permettra au programme appelant d'omettre la clause `CATCH` superflue.

#### Procédez à un vidage de la mémoire pour les situations totalement irrécupérables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#procédez-à-un-vidage-de-la-mémoire-pour-les-situations-totalement-irrécupérables)

Si une situation est grave au point que vous savez pertinemment que le programme appelant a peu de chances de s'en remettre, ou si cette situation indique clairement une erreur de programmation, procédez à un vidage de la mémoire plutôt que de lever une exception : échec d'acquisition de la mémoire, échec des lectures d'index sur une table qui doit être renseignée, etc.

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

Ce comportement empêchera tous les consommateurs, quels qu'ils soient, de faire quelque chose d'utile par la suite. Utilisez cette solution uniquement si vous êtes certain.

#### Préférez RAISE EXCEPTION NEW à RAISE EXCEPTION TYPE

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Levée d'exceptions](#levée-dexceptions) > [Cette section](#préférez-raise-exception-new-à-raise-exception-type)

Remarque : disponible dans NW 7.52 et les versions ultérieures.

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

est plus court en général que l'exemple inutilement plus long

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

Cependant, si vous utilisez massivement l'option `MESSAGE`, il est recommandé de s'en tenir à la variante `TYPE` :

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception
  MESSAGE e136(messages).
```

### Interception

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Cette section](#interception)

#### Enveloppez les exceptions externes pour éviter qu'elles n'envahissent votre code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Gestion des erreurs](#gestion-des-erreurs) > [Interception](#interception) > [Cette section](#enveloppez-les-exceptions-externes-pour-éviter-quelles-nenvahissent-votre-code)

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

La [loi de Déméter](https://en.wikipedia.org/wiki/Law_of_Demeter) recommande de découpler les éléments. La transmission d'exceptions provenant d'autres composants viole ce principe. Faites en sorte d'être indépendant vis-à-vis du code externe en interceptant ces exceptions et en les enveloppant dans un type d'exception à vous.

```ABAP
" anti-pattern
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## Commentaires

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#commentaires)

### Exprimez-vous via le code, et non via des commentaires

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#exprimez-vous-via-le-code-et-non-via-des-commentaires)

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

au lieu de

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

La méthodologie Clean Code ne vous interdit _pas_ de commenter votre code. Simplement, il vous encourage à exploiter de _meilleurs_ moyens et à recourir aux commentaires uniquement si ces autres moyens ne conviennent pas.

> L'exemple ci-dessus a été controversé d'un point de vue des performances, certains ayant déclaré que le découpage des méthodes en si petites parties entraînait une trop forte baisse des performances. D'après des exemples de mesures, le code refactorisé est 2,13 fois plus lent que sa variante "sale" d'origine. La variante propre corrige l'entrée `31-02-2018` en 9,6 microsecondes, tandis que la variante "sale" le fait en seulement 4,5 microsecondes. Cela peut poser problème lorsque la méthode est exécutée très fréquemment dans une application haute performance ; pour la validation normale des entrées utilisateur, ce temps supplémentaire est acceptable. Reportez-vous à la section [Tenez compte des performances](#tenez-compte-des-performances) pour traiter du Clean Code et des problèmes de performances.

### Les commentaires ne sont pas une excuse devant justifier les noms inappropriés

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#les-commentaires-ne-sont-pas-une-excuse-devant-justifier-les-noms-inappropriés)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

Attribuez des noms plus pertinents plutôt que d'expliquer ce qu'ils signifient en réalité ou la raison pour laquelle vous avez choisi des noms inappropriés.

```ABAP
" anti-pattern
" checks whether the table input contains entries
DATA(result) = check_table( input ).
```

### Utilisez les méthodes plutôt que les commentaires pour segmenter votre code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#utilisez-les-méthodes-plutôt-que-les-commentaires-pour-segmenter-votre-code)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

En plus de rendre plus claires l'intention, la structure et les dépendances du code, cela évite aussi les erreurs de transfert lorsque des variables temporaires ne sont pas réinitialisées entre les sections.

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

### Écrivez des commentaires pour répondre à la question "pourquoi ?", et non à la question "quoi ?"

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#écrivez-des-commentaires-pour-répondre-à-la-question-pourquoi--et-non-à-la-question-quoi-)

```ABAP
" can't fail, existence of >= 1 row asserted above
DATA(first_line) = table[ 1 ].
```

Personne n'a besoin d'un code répété en langage naturel

```ABAP
" anti-pattern
" select alert root from database by key
SELECT * FROM d_alert_root WHERE key = key.
```

### La conception doit être traitée dans les documents de conception, et non dans le code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#la-conception-doit-être-traitée-dans-les-documents-de-conception-et-non-dans-le-code)

```ABAP
" anti-pattern
" This class serves a double purpose. First, it does one thing. Then, it does another thing.
" It does so by executing a lot of code that is distributed over the local helper classes.
" To understand what's going on, let us at first ponder the nature of the universe as such.
" Have a look at this and that to get the details.
```

Personne ne lit ça, sérieusement. Si les collaborateurs ont besoin d'un manuel pour pouvoir utiliser votre code, cela peut être un signe indiquant que votre code présente de gros problèmes de conception, que vous devriez résoudre autrement. Si du code a _vraiment_ besoin d'explications allant au-delà d'une simple ligne de commentaires, pensez à lier le document de conception à ces cas.

### Commentez avec ", et non avec *

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#commentez-avec--et-non-avec)

Les commentaires avec guillemets se mettent en retrait en alignement avec les instructions qu'ils commentent

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

Les commentaires avec astérisques ont tendance se mettre en retrait n'importe comment

```ABAP
" anti-pattern
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### Mettez des commentaires avant l'instruction à laquelle ils font référence

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#mettez-des-commentaires-avant-linstruction-à-laquelle-ils-font-référence)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

Plus propre que

```ABAP
" anti-pattern
output = calculate_result( input ).
" delegate pattern
```

Et moins invasif que

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### Supprimez le code au lieu de le commenter

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#supprimez-le-code-au-lieu-de-le-commenter)

```ABAP
" anti-pattern
* output = calculate_result( input ).
```

Lorsque vous trouvez des choses de ce genre, supprimez-les. Manifestement le code n'est pas nécessaire, puisque votre application fonctionne et que tous les tests sont au vert. Le code supprimé pourra être reproduit ultérieurement à partir de l'historique des versions. Si vous avez besoin de conserver un morceau de code de manière permanente, copiez-le dans un fichier ou dans un objet `$TMP` ou `HOME`.

### Utilisez FIXME, TODO et XXX, et ajoutez votre ID

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#utilisez-fixme-todo-et-xxx-et-ajoutez-votre-id)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME` pointe des erreurs qui sont trop petites ou trop en gestation pour des incidents internes.
- Les `TODO` sont des endroits où vous voulez ajoutez quelque chose dans un avenir proche (!).
- `XXX` marque du code qui fonctionne, mais qui pourrait être meilleur.

Lorsque vous saisissez un tel commentaire, ajoutez votre surnom, vos initiales ou votre utilisateur pour permettre à vos codéveloppeurs de vous contacter et de vous poser des questions si le commentaire n'est pas clair.

### N'ajoutez pas de commentaires de type signature de méthode et end-of

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#najoutez-pas-de-commentaires-de-type-signature-de-méthode-et-end-of)

Les commentaires de type signature de méthode n'aident personne.

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

Il y a quelques décennies, quand vous ne pouviez pas voir la signature de méthode lors de l'inspection de son code ou quand vous travailliez avec des impressions qui faisaient des douzaines de pages, ces commentaires pouvaient avoir un sens. Mais tous les IDE ABAP modernes (SE24, SE80, ADT) affichent facilement la signature de méthode, et ces commentaires sont donc devenus des commentaires parasites.

> Dans l'éditeur basé sur des formulaires de SE24/SE80, appuyez sur le bouton _Signature_. Dans ABAP Development Tools, marquez le nom de la méthode et appuyez sur F2 ou ajoutez la vue _Info élément ABAP_ à votre perspective.

De même les commentaires end-of sont superflus. Ces commentaires ont pu être utiles il y a quelques décennies, quand les programmes et fonctions et les IF qui y étaient imbriqués faisaient des centaines de lignes de code. Mais notre style de code moderne produit des méthodes suffisamment courtes pour voir à la lecture à quelle instruction d'ouverture un `ENDIF` ou `ENDMETHOD` appartient :

```ABAP
" anti-pattern
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### N'ajoutez pas des commentaires qui font doublons avec les textes de messages

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#najoutez-pas-des-commentaires-qui-font-doublons-avec-les-textes-de-messages)

```ABAP
" anti-pattern
" alert category not filled
MESSAGE e003 INTO dummy.
```

Les messages changent indépendamment de votre code, et personne ne pensera à adapter le commentaire, de sorte qu'il va rapidement se périmer et même devenir trompeur sans que personne ne le remarque.

Les IDE modernes fournissent des moyens aisés de voir le texte derrière un message ; par exemple, dans les ABAP Development Tools, marquez l'ID de message et appuyez sur Maj+F2.

Si vous voulez que ce soit plus explicite, pensez à extraire le message dans une méthode dédiée.

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### Utilisez ABAP Doc uniquement pour les API publiques

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#utilisez-abap-doc-uniquement-pour-les-api-publiques)

Écrivez dans ABAP Doc pour documenter des API publiques, c'est-à-dire des API qui sont prévues pour des développeurs d'autres équipes ou application. N'écrivez pas dans ABAP Doc pour du contenu interne.

ABAP Doc souffre des mêmes faiblesses que tous les commentaires, à savoir qu'il se périme très vite et devient ensuite trompeur. En conséquence, vous devez l'employer uniquement où cela a du sens. N'imposez pas d'écrire dans ABAP Doc pour tout et n'importe quoi.

> Pour en savoir plus, lisez _Chapitre 4 : Bons commentaires : documentation Javadoc dans les API publiques_ et _Chapitre 4 : Mauvais commentaires : documentation Javadoc dans du code non public_ dans le livre de [Robert C. Martin _Coder proprement_].

### Préférez les pragmas aux pseudo-commentaires

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Commentaires](#commentaires) > [Cette section](#préférez-les-pragmas-aux-pseudo-commentaires)

Préférez les pragmas aux pseudo-commentaires pour supprimer les avertissements et erreurs non pertinents identifiés par l'ATC. Les pseudo-commentaires sont généralement devenus obsolètes et ont été remplacés par des pragmas.

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" anti-pattern
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

Utilisez le programme `ABAP_SLIN_PRAGMAS` ou la table `SLIN_DESC` pour rechercher le mappage entre les pseudo-commentaires et les pragmas qui les ont remplacés.

## Mise en forme

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#mise-en-forme)

Les suggestions ci-dessous sont [optimisées pour la lecture, et non l'écriture](#optimisez-votre-code-pour-la-lecture-et-non-lécriture). Comme Pretty Printer d'ABAP ne les couvre pas, certaines d'entre elles produisent du travail manuel supplémentaire de nouvelle mise en forme d'instructions lorsque les longueurs des noms etc. changent ; si vous voulez éviter cela, pensez à abandonner des règles telles que [Alignez les affectations sur le même objet, et non sur des objets différents](#alignez-les-affectations-sur-le-même-objet-et-non-sur-des-objets-différents).

### Soyez cohérent

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#soyez-cohérent)

Mettez en forme tout le code d'un projet de la même manière. Faites en sorte que tous les membres de l'équipe utilisent le même style de mise en forme.

Si vous traitez du code externe, conformez-vous au style de mise en forme de ce projet plutôt que de persister avec votre style personnel.

Si vous modifier vos règles de mise en forme au cours du temps, utilisez les [bonnes pratiques de refactoring](#refactoriser-du-code-existant) pour mettre à jour votre code.

### Optimisez votre code pour la lecture, et non l'écriture

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#optimisez-votre-code-pour-la-lecture-et-non-lécriture)

Les développeurs passent beaucoup de temps à _lire_ du code. En réalité, _écrire_ du code occupe une plus petite partie de leur journée.

En conséquence, vous devez optimiser votre mise en forme du code pour la lecture et le débogage, et non pour l'écriture.

Par exemple, vous devez préférer

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

pour coder quelque chose comme

```ABAP
" anti-pattern
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### Utilisez la fonction Pretty Printer avant l'activation

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#utilisez-la-fonction-pretty-printer-avant-lactivation)

Appliquez Pretty Printer - Maj+F1 dans SE80, SE24 et ADT - avant d'activer un objet.

Si vous modifiez une grosse base de code hérité non mis en forme, il se peut que vous souhaitiez appliquer Pretty Printer seulement pour des lignes sélectionnées, pour éviter d'énormes listes de modifications et dépendances de transport. Pensez à utiliser Pretty Printer sur l'objet de développement complet dans un ordre de transport ou une note distinct(e).

> Pour en savoir plus, lisez _Chapitre 5 : Mise en forme : règles d'une équipe_ dans le livre de [Robert C. Martin _Coder proprement_].

### Utilisez les options Pretty Printer paramétrées pour votre équipe

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#utilisez-les-options-pretty-printer-paramétrées-pour-votre-équipe)

Utilisez toujours les options de votre équipe. Indiquez-les sous _Menu_ > _Utilitaires_ > _Options..._ > _Éditeur ABAP_ > _Pretty Printer_.

Définissez _Retrait_ et _Convertir majuscules/minuscules_ > _Mot-clé en majuscules_ comme il est convenu dans votre équipe.

> [Majuscules vs minuscules](sub-sections/UpperVsLowerCase.md) explique pourquoi nous ne donnons pas de directive claire sur la casse des mots-clés.
Pour en savoir plus, lisez _Chapitre 5 : Mise en forme : règles d'une équipe_ dans le livre de [Robert C. Martin _Coder proprement_].

### Pas plus d'une instruction par ligne

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#pas-plus-dune-instruction-par-ligne)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

Même si certaines occurrences peuvent vous faire croire à tort que c'est lisible :

```ABAP
" anti-pattern
DATA do_this TYPE i. do_this = input + 3.
```

### Gardez une longueur de ligne raisonnable

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#gardez-une-longueur-de-ligne-raisonnable)

Conformez-vous à une longueur de ligne de 120 caractères maximum.

L'œil humain lit un texte avec plus de confort si les lignes ne sont pas trop larges. Posez la question à un concepteur d'IU ou à un chercheur spécialiste en mouvements oculaires. Vous apprécierez aussi un code moins large lorsque vous effectuerez du débogage ou une comparaison de deux sources entre elles.

La limite de 80 ou même de 72 caractères, qui a pour origine les anciens terminaux, est un peu trop restrictive. 100 caractères sont souvent recommandés et constituent un choix viable, mais il semble qu'une limite de 120 caractères fonctionne un peu mieux pour ABAP, peut-être en raison de la verbosité générale du langage.

> Comme rappel, vous pouvez configurer la marge d'impression dans ADT à 120 caractères, elle apparaît alors dans la vue de code comme une ligne verticale. Configurez cela sous _Menu_ > _Fenêtre_ > _Préférences_ > _Général_ > _Éditeurs_ > _Éditeurs de texte_.

### Condensez votre code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#condensez-votre-code)

```ABAP
DATA(result) = calculate( items ).
```

au lieu d'ajouter des espaces superflus

```ABAP
" anti-pattern
DATA(result)        =      calculate(    items =   items )   .
```

### Ajoutez juste une ligne vierge pour séparer les différentes parties, pas plus

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#ajoutez-juste-une-ligne-vierge-pour-séparer-les-différentes-parties-pas-plus)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

pour mettre en évidence que les deux instructions font des choses différentes. Mais il n'y a aucune justification à

```ABAP
" anti-pattern
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

L'empressement à ajouter des lignes vierges peut être un signe indiquant que votre méthode [ne fait pas qu'une seule chose](#faites-une-chose-faites-la-bien-et-ne-faites-que-cela).

### N'abusez pas des lignes vierges de séparation

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#nabusez-pas-des-lignes-vierges-de-séparation)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

Aucune raison ne vient justifier la mauvaise habitude d'éparpiller votre code avec des lignes vierges

```ABAP
" anti-pattern
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

En réalité, les lignes vierges ont un sens uniquement si vous avez des instructions qui s'étendent sur plusieurs lignes

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

### Alignez les affectations sur le même objet, et non sur des objets différents

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#alignez-les-affectations-sur-le-même-objet-et-non-sur-des-objets-différents)

Pour mettre en évidence que ces éléments vont ensemble

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

ou encore mieux

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

Mais laissez dépareillés les éléments qui n'ont rien à voir entre eux :

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> Pour en savoir plus, lisez _Chapitre 5 : Mise en forme : alignement horizontal_ dans le livre de [Robert C. Martin _Coder proprement_].

### Fermez les guillemets à la fin de la ligne

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#fermez-les-guillemets-à-la-fin-de-la-ligne)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### Faites en sorte que les appels de paramètre unique soient sur une seule ligne

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#faites-en-sorte-que-les-appels-de-paramètre-unique-soient-sur-une-seule-ligne)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

au lieu de la version inutilement plus longue

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### Faites en sorte que les paramètres apparaissent derrière l'appel

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#faites-en-sorte-que-les-paramètres-apparaissent-derrière-lappel)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Lorsque cela rend les lignes très longues, vous pouvez revenir à la ligne pour les paramètres :

```ABAP
DATA(sum) = add_two_numbers(
                   value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                   value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### Si vous revenez à la ligne, mettez les paramètres en retrait sous l'appel

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#si-vous-revenez-à-la-ligne-mettez-les-paramètres-en-retrait-sous-lappel)

```ABAP
DATA(sum) = add_two_numbers(
                   value_1 = 5
                   value_2 = 6 ).
```

Il est plus difficile d'identifier ce à quoi ils appartiennent lorsque les paramètres sont alignés ailleurs.

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

Cependant, il s'agit du meilleur modèle pour éviter que la mise en forme soit affectée par une variation de longueur du nom.

### En cas de multiples paramètres, revenez à la ligne

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#en-cas-de-multiples-paramètres-revenez-à-la-ligne)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Oui, ça prend beaucoup de place. Cependant, il est difficile sinon de repérer où un paramètre finit et où le suivant commence :

```ABAP
" anti-pattern
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### Alignez les paramètres

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#alignez-les-paramètres)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

Avec des marges dépareillées, il est difficile de repérer où le paramètre finit et où sa valeur commence :

```ABAP
" anti-pattern
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> D'un autre côté, il s'agit du meilleur modèle pour éviter que la mise en forme soit affectée par une variation de longueur du nom.

### Mettez l'appel sur deux lignes si la ligne initiale est trop longue

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#mettez-lappel-sur-deux-lignes-si-la-ligne-initiale-est-trop-longue)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### Ajoutez un retrait et passez-le en tabulation

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#ajoutez-un-retrait-et-passez-le-en-tabulation)

Mettez en retrait les mots-clés de paramètres de 2 espaces et les paramètres de 4 espaces :

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

Si vous n'avez pas de mot-clé, mettez en retrait les paramètres de 4 espaces.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Utilisez la touche de tabulation pour la mise en retrait. Ce n'est pas un problème si cela ajoute un espace de plus que nécessaire. (Cela arrive si la partie `DATA(sum) =` à gauche a un nombre impair de caractères.)

### Mettez les déclarations en ligne en retrait comme les appels de méthode

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#mettez-les-déclarations-en-ligne-en-retrait-comme-les-appels-de-méthode)

Mettez en retrait les déclarations en ligne avec VALUE ou NEW comme s'il s'agissait d'appels de méthode :

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### N'alignez pas les clauses type

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Mise en forme](#mise-en-forme) > [Cette section](#nalignez-pas-les-clauses-type)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

Une variable et son type vont ensemble et doivent donc apparaître groupés l'une à côté de l'autre. Aligner les clauses `TYPE` détourne l'attention et suggère que les variables forment un groupe vertical, et leurs types un autre. L'alignement produit également une charge de traitement inutile, car il vous demande d'ajuster tous les retraits lorsque la longueur de la plus longue variable change.

```ABAP
" anti-pattern
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

## Test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Cette section](#test)

### Principes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#principes)

#### Écrivez des codes testables

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Principes](#principes) > [Cette section](#écrivez-des-codes-testables)

Écrivez tout le code d'une manière qui vous permette de le tester de façon automatique.

Si cela requiert de refactoriser votre code, faites-le. Faites-le d'abord, avant de commencer à ajouter de nouvelles fonctionnalités.

Si vous effectuez des ajouts à du code hérité qui est trop mal structuré pour être testé, refactorisez-le au moins de manière à pouvoir tester vos ajouts.

#### Laissez les autres faire des simulations de votre code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Principes](#principes) > [Cette section](#laissez-les-autres-faire-des-simulations-de-votre-code)

Si vous écrivez du code utilisé par d'autres, permettez-leur d'écrire des tests de modules pour leur propre code, par exemple en ajoutant des interfaces à tous les endroits ouverts sur l'extérieur, en fournissant des simulations de test utiles qui facilitent les tests d'intégration ou en appliquant l'inversion des dépendances pour leur permettre de substituer la configuration productive avec une configuration test.

#### Règles relatives à la lisibilité

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Principes](#principes) > [Cette section](#règles-relatives-à-la-lisibilité)

Rendez votre code test encore plus lisible que votre code productif. Vous pouvez surmonter un code productif mauvais avec de bons tests, mais si vous n'obtenez même pas les tests, vous êtes perdu.

Gardez votre code test assez simple et bête pour pouvoir encore le comprendre dans un an.

Tenez-vous en à des standard et à des modèles, afin de permettre à vos collègues de rentrer rapidement dans le code.

#### Ne faites pas de copies et n'écrivez pas de programmes de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Principes](#principes) > [Cette section](#ne-faites-pas-de-copies-et-nécrivez-pas-de-programmes-de-test)

Ne commencez pas à travailler sur un poste de consignation en effectuant une copie `$TMP` d'un objet de développement et à jongler avec. Les autres ne remarqueront pas ces objets et ne connaitront donc pas le statut de votre travail. Vous allez probablement gaspiller beaucoup de temps à faire des copies de travail en premier lieu. Vous allez aussi oublier de supprimer les copies après coup, polluant votre système et leurs dépendances. (Vous n'y croyez pas ? Allez dans votre système de développement et contrôlez vos `$TMP` tout de suite.)

Ne commencez pas non plus par écrire un programme de test qui appelle quelque chose d'une certaine manière, et le répète pour vérifier que les choses fonctionnent encore lorsque vous travaillez dessus. C'est le test du pauvre : répéter un rapport de test à la main et vérifier de visu si tout va toujours bien. Passez à l'étape suivante et automatisez ce programme dans un test de module, avec une assertion automatique qui vous indique si le code est toujours correct. Premièrement, vous vous épargnerez l'effort d'écrire les tests de module après coup. Deuxièmement, vous ne perdrez pas de temps avec des répétitions manuelles, en plus d'éviter l'ennui et la fatigue.

#### Testez les parties publiques, et non les parties internes privées

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Principes](#principes) > [Cette section](#testez-les-parties-publiques-et-non-les-parties-internes-privées)

Les parties publiques des classes, en particulier les interfaces qu'elles implémentent, sont plutôt stables et ont peu de probabilités de changer. Faites en sorte que vos tests de module valident seulement les parties publiques pour les rendre fiables et pour minimiser l'effort que vous aurez à fournir lors du refactoring de la classe. Les parties internes protégées et privées, à l'inverse, peuvent changer très rapidement via le refactoring, de telle sorte que vos tests ne fonctionneront plus à chaque refactoring.

Un besoin urgent de tester des méthodes privées ou protégées peut être une alerte précoce de plusieurs sortes de failles de conception. Posez-vous la question :

- Avez-vous accidentellement enterré dans votre classe un concept qui veut apparaître dans sa propre classe, avec sa propre suite de tests dédiée ?

- Avez-vous oublié de séparer la logique de domaine du code de liaison ? Par exemple, implémenter la logique de domaine directement dans la classe qui est raccordée dans BOPF comme une action, une détermination ou une validation, ou qui a été générée par la passerelle SAP comme fournisseur de données `*_DPC_EXT`, n'est peut-être pas la meilleure idée.

- Vos interfaces sont-elles trop compliquées et demandent-elles trop de données qui ne sont pas pertinentes ou qui ne peuvent pas être simulées aisément ?

#### Ne soyez pas obsédé par la couverture du code

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Principes](#principes) > [Cette section](#ne-soyez-pas-obsédé-par-la-couverture-du-code)

La couverture du code est là pour vous aider à rechercher du code que vous avez oublié de tester, pas pour satisfaire de quelconques KPI :

N'inventez pas des tests avec ou sans asserts fictifs juste pour une meilleure couverture du code. Il vaut mieux laisser des choses non testées pour montrer de manière transparente que vous ne pouvez pas les refactoriser sans risque. Vous pouvez avoir une couverture inférieure à 100 % et avoir quand même des tests parfaits. Il y a des cas (comme des IF dans le constructeur pour insérer des simulations de test) qui rendent impossible en pratique l'atteinte des 100 %. De bons tests tendent à couvrir la même instruction plusieurs fois, pour différents branchements et différentes conditions. Ils ont en fait une couverture du code théoriquement supérieure à 100 %.

### Classes de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#classes-de-test)

#### Appelez les classes de test locales en fonction de leur objectif

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Classes de test](#classes-de-test) > [Cette section](#appelez-les-classes-de-test-locales-en-fonction-de-leur-objectif)

```ABAP
CLASS ltc_unit_tests DEFINITION FOR TESTING ... .
CLASS ltc_integration_tests DEFINITION FOR TESTING ... .
CLASS ltc_unit_tests_with_mocks DEFINITION FOR TESTING ... .
```

Les noms appropriés révèlent le niveau des tests et ce qui est commun à leur configuration.

```ABAP
" anti-patterns
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " We know that's the class under test - why repeat it?
CLASS ltc_test DEFINITION FOR TESTING ....                      " Of course it's a test, what else should it be?
```

#### Mettez les tests dans les classes locales

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Classes de test](#classes-de-test) > [Cette section](#mettez-les-tests-dans-les-classes-locales)

Mettez les tests de module dans l'include de test local de la classe testée. Ainsi, les collaborateurs peuvent retrouver ces tests lors du refactoring de la classe, ce qui leur permet d'exécuter tous les tests associés en appuyant sur une touche seulement, tel que décrit dans [Comment exécuter des classes de test](#comment-exécuter-des-classes-de-test).

Mettez les tests de composant, d'intégration et de système dans l'include de test local d'une classe globale distincte. Ils ne font pas directement référence à une classe individuelle testée. Par conséquent, ils ne doivent pas être placés arbitrairement dans une des classes concernées, mais dans une classe distincte. Marquez cette classe de test globale comme `FOR TESTING` et `ABSTRACT` afin d'éviter qu'elle soit accidentellement référencée dans le code productif. Si les tests sont mis dans d'autres classes, les collaborateurs risquent de les négliger et d'oublier de les exécuter lors du refactoring des classes concernées.

Par conséquent, il est bénéfique d'utiliser des *relations de test* pour documenter tous les objets ayant passé le test. Dans l'exemple ci-dessous, la classe de test `hiring_test` pourrait être exécutée bien qu'elle soit dans la classe `recruting` ou `candidate` via le raccourci `Shift-Crtl-F12` (Windows) ou `Cmd-Shift-F12` (macOS).

```abap
"! @testing recruting
"! @testing candidate
class hiring_test defintion
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### Mettez les méthodes d'aide dans les classes d'aide

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Classes de test](#classes-de-test) > [Cette section](#mettez-les-méthodes-daide-dans-les-classes-daide)

Mettez les méthodes d'aide utilisées par plusieurs classes de test dans une classe d'aide. Garantissez la disponibilité des méthodes d'aide via l'héritage (relation is-a) ou la délégation (relation has-a).

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

#### Comment exécuter des classes de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Classes de test](#classes-de-test) > [Cette section](#comment-exécuter-des-classes-de-test)

Dans ABAP Development Tools, appuyez sur Ctrl+Shift+F10 pour exécuter tous les tests dans une classe. Appuyez sur Ctrl+Shift+F11 pour inclure les mesures de couverture du code. Appuyez sur Ctrl+Shift+F12 pour exécuter également les tests figurant dans d'autres classes gérées en tant que relations de test.

> Sur macOS, utilisez `Cmd` au lieu de `Ctrl`.

### Membre testé

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#membre-testé)

#### Donnez un nom explicite au membre testé ou dénommez-le CUT par défaut

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Membre testé](#membre-testé) > [Cette section](#donnez-un-nom-explicite-au-membre-testé-ou-dénommez-le-cut-par-défaut)

Donnez un nom explicite à la variable qui représente le membre testé :

```ABAP
DATA blog_post TYPE REF TO ...
```

Ne vous contentez pas de répéter simplement le nom de classe avec tous ses espaces noms et ses préfixes :

```ABAP
" anti-pattern
DATA clean_fra_blog_post TYPE REF TO ...
```

Si vous avez plusieurs configurations de test, il peut être utile de décrire le statut changeant de l'objet :

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

Si vous peinez à trouver un nom explicite, recourez à `cut` comme valeur par défaut. Cette abréviation signifie "membre testé" (pour "code under test" en anglais).

```ABAP
DATA cut TYPE REF TO ...
```

Dans les tests non propres et déroutants en particulier, l'appel de la variable `cut` peut temporairement aider le lecteur à voir ce qui est réellement testé. Cependant, nettoyer les tests s'avère la méthode à suivre sur le long terme.

#### Testez des interfaces, pas des classes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Membre testé](#membre-testé) > [Cette section](#testez-des-interfaces-pas-des-classes)

Conséquence pratique de la règle [_Testez les parties publiques, et non les parties internes privées_](#testez-les-parties-publiques-et-non-les-parties-internes-privées), vous devez typer votre membre testé avec une _interface_

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

plutôt qu'avec une _classe_

```ABAP
" anti-pattern
DATA code_under_test TYPE REF TO some_class.
```

#### Extrayez l'appel au membre testé dans sa propre méthode

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Membre testé](#membre-testé) > [Cette section](#extrayez-lappel-au-membre-testé-dans-sa-propre-méthode)

Si la méthode à tester requiert un grand nombre de paramètres ou des données préparées, cette règle peut vous aider à extraire l'appel au membre testé dans une méthode du composant d'aide lui étant propre qui définit par défaut les paramètres inintéressants :

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

En appelant directement la méthode d'origine, vous risquez de "noyer" votre test dans une masse de détails insignifiants :

```ABAP
" anti-pattern
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### Injection

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#injection)

#### Utilisez l'inversion des dépendances pour injecter des simulations test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#utilisez-linversion-des-dépendances-pour-injecter-des-simulations-test)

L'inversion des dépendances signifie le transfert de toutes les dépendances vers le constructeur :

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

N'utilisez pas l'injection setter. Elle permet l'utilisation du code productif de façons non prévues :

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

N'utilisez pas l'injection FRIENDS. Elle initialisera les dépendances productives avant qu'elles ne soient remplacées, entraînant potentiellement des conséquences inattendues. Elle ne fonctionnera plus dès que vous renommerez les parties internes. Elle contourne également les initialisations dans le constructeur.

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

#### Pensez à utiliser l'outil simulation test ABAP

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#pensez-à-utiliser-loutil-simulation-test-abap)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

est plus court et plus facile à comprendre que les simulations test personnalisées :

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

#### Exploitez les outils de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#exploitez-les-outils-de-test)

En général, pour un style de programmation propre, vous effectuerez la plupart du travail avec les tests de modules et les simulations test ABAP standard. Cependant, il existe des outils qui vous permettront d'aborder des cas plus ardus, tout en faisant preuve d'élégance :

- Utilisez le service `CL_OSQL_REPLACE` pour tester les instructions OpenSQL complexes en les redirigeant vers une corbeille de données de test qui peut être remplie par des données de test sans interférer avec le reste du système.

- Utilisez le framework de test CDS pour tester vos vues CDS.

#### Utilisez les test seams comme solution de contournement temporaire

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#utilisez-les-test-seams-comme-solution-de-contournement-temporaire)

Si toutes les autres techniques échouent ou si vous jouez sur le terrain glissant du code existant, évitez les [test seams](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm) pour rendre les éléments testables.

Les test seams ont l'air pratiques à première vue, mais ils sont envahissants et tendent à créer un méli-mélo dans les dépendances privées. Il est donc difficile de les maintenir actives et stables sur le long terme.

C'est pourquoi nous vous recommandons de recourir aux test seams uniquement comme solution de contournement temporaire, pour que vous puissiez refactoriser le code en un format plus testable.

#### Utilisez LOCAL FRIENDS pour accéder au constructeur d'inversion des dépendances

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#utilisez-local-friends-pour-accéder-au-constructeur-dinversion-des-dépendances)

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

#### N'utilisez pas LOCAL FRIENDS à mauvais escient pour envahir le code testé

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#nutilisez-pas-local-friends-à-mauvais-escient-pour-envahir-le-code-testé)

Les tests de module qui accèdent aux membres privés et protégés pour insérer des données de simulation sont fragiles : ils ne fonctionnent plus lorsque la structure interne du code testé évolue.

```ABAP
" anti-pattern
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### Ne modifiez pas le code productif pour rendre le code testable

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#ne-modifiez-pas-le-code-productif-pour-rendre-le-code-testable)

```ABAP
" anti-pattern
IF me->in_test_mode = abap_true.
```

#### Ne créez pas des sous-classes pour simuler des méthodes

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#ne-créez-pas-des-sous-classes-pour-simuler-des-méthodes)

Ne placez pas les méthodes dans des sous-classes et n'écrasez pas les méthodes pour les simuler dans vos tests de module. Bien que cette solution fonctionne, elle est fragile, car souvent, les tests ne fonctionnent plus suite au refactoring du code. De plus, elle permet aux consommateurs réels d'hériter de votre classe, ce [qui peut être source de mauvaises surprises si vous ne la concevez pas explicitement pour l'héritage](#final-si-non-conçue-pour-lhéritage).

```ABAP
" anti-pattern
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

Pour tester le code existant, [recourez plutôt aux test seams](#utilisez-les-test-seams-comme-solution-de-contournement-temporaire). Ils sont tout aussi fragiles, mais demeurent la solution la plus propre, car ils ont l'avantage de ne pas modifier le comportement productif de la classe, contrairement à l'activation de l'héritage via le retrait d'un indicateur `FINAL` précédent ou la modification de la portée de la méthode de `PRIVATE` en `PROTECTED`.

Lorsque vous écrivez du nouveau code, tenez compte des problèmes de testabilité directement lors de la conception de la classe et trouvez une autre solution qui s'avèrera meilleure. Le [recours à d'autres outils de test](#exploitez-les-outils-de-test) et l'extraction de la méthode du problème dans une classe distincte avec sa propre interface figurent parmi les meilleures pratiques les plus courantes.

> Une variante plus spécifique par rapport à [Ne modifiez pas le code productif pour rendre le code testable](#ne-modifiez-pas-le-code-productif-pour-rendre-le-code-testable)

#### Ne simulez pas ce dont vous n'avez pas besoin

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#ne-simulez-pas-ce-dont-vous-navez-pas-besoin)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

Définissez vos "given" aussi précisément que possible : ne définissez pas des données dont votre test n'a pas besoin et ne simulez pas les objets qui ne sont jamais appelé. Ces éléments ont pour effet de détourner l'attention du lecteur de ce qu'il se passe réellement.

```ABAP
" anti-pattern
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

Dans certains cas, il n'est même pas nécessaire de simuler quoi que ce soit. Cela arrive souvent avec les structures de données et les conteneurs de données. Par exemple, vos tests de module peuvent très bien fonctionner avec la version productive d'un `transient_log`, car celui-ci ne stocke que des données sans effets secondaires.

#### Ne créez pas de framework de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Injection](#injection) > [Cette section](#ne-créez-pas-de-framework-de-test)

Les tests de module, contrairement aux tests d'intégration, doivent être de type data-in-data-out (données entrantes, données sortantes), avec toutes les données de test définies à la volée, au besoin.

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

Ne commencez pas à créer des frameworks qui distinguent les "*ID de scénarios de test*" pour décider les données à fournir. Le code qui en résultera sera si long et confus que vous ne serez pas en mesure de maintenir ces tests actifs sur le long terme.

```ABAP
" anti-pattern

test_double->set_test_case( 1 ).

CASE me->test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### Méthodes de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#méthodes-de-test)

#### Noms de méthode de test : reflètent ce qui est donné et attendu

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Méthodes de test](#méthodes-de-test) > [Cette section](#noms-de-méthode-de-test--reflètent-ce-qui-est-donné-et-attendu)

Les noms appropriés reflètent les "given" et "then" du test :

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

Les noms appropriés reflètent le "when", répètent des faits insignifiants ou sont énigmatiques :

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

Comme ABAP n'autorise que 30 caractères pour les noms de méthode, il est légitime d'ajouter un commentaire explicatif si le nom est trop court pour véhiculer assez de sens. Il peut être approprié de choisir ABAP Doc ou la première ligne dans la méthode de test comme commentaire.

Si vous avez de nombreuses méthodes de test aux noms trop longs, c'est peut-être un signe indiquant que vous devez fractionner votre classe de test individuelle en plusieurs et exprimer les différences au travers des "given" dans les noms de classe.

#### Utilisez le format given-when-then

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Méthodes de test](#méthodes-de-test) > [Cette section](#utilisez-le-format-given-when-then)

Organisez votre code test selon le paradigme given-when-then : premièrement, initialisez les éléments dans une section donnée ("given") ; deuxièmement, appelez l'élément réellement testé ("when") ; troisièmement, validez le résultat ("then").

Si les sections "given" ou "then" deviennent si longues que vous ne pouvez plus séparer visuellement les trois sections, extrayez des sous-méthodes. Les lignes vierges ou les commentaires utilisés comme séparateurs font bon effet au premier abord, mais ne corrigent pas vraiment l'aspect fouillis au niveau visuel. Néanmoins, ceux-ci sont utiles du point de vue du lecteur et du rédacteur de test novice pour séparer les sections.

#### "When" est un appel et un seul

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Méthodes de test](#méthodes-de-test) > [Cette section](#when-est-un-appel-et-un-seul)

Veillez à ce que la section "when" de votre méthode de test contienne un appel et un seul à la classe testée :

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

L'appel de plusieurs éléments indique que la méthode n'a pas une cible précise et teste trop d'éléments. Dans ce contexte, il est plus difficile de trouver la cause de l'échec du test : est-ce le premier, le deuxième ou le troisième appel qui a causé l'échec ? Le lecteur est également dérouté, car il ne sait pas exactement quelle est la fonctionnalité testée.

#### N'ajoutez pas un TEARDOWN à moins d'en avoir vraiment besoin

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Méthodes de test](#méthodes-de-test) > [Cette section](#najoutez-pas-un-teardown-à-moins-den-avoir-vraiment-besoin)

Les méthodes `teardown` sont généralement nécessaires uniquement pour nettoyer les entrées dans la base de données ou d'autres ressources externes dans les tests d'intégration.

La redéfinition des membres de la classe de test, en particulier `cut` et les simulations test utilisées, est superflue ; ils sont écrasés par la méthode `setup` avant le lancement de la méthode de test suivante.

### Données de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#données-de-test)

#### Facilitez la lecture et la compréhension

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Données de test](#données-de-test) > [Cette section](#facilitez-la-lecture-et-la-compréhension)

Dans les tests de module, vous souhaitez pouvoir repérer rapidement les données et simulations qui sont importantes, et celles qui sont là uniquement pour éviter le plantage du code. Pour faciliter la lecture, donnez des noms et valeurs clairs aux éléments dépourvus de signification, par exemple :

```ABAP
DATA(alert_id) = '42'.                             " well-known meaningless numbers
DATA(detection_object_type) = '?=/"&'.             " 'keyboard accidents'
CONSTANTS some_random_number TYPE i VALUE 782346.  " revealing variable names
```

Ne piégez pas les collaborateurs en leur faisant croire que quelque chose est en lien avec des objets réels ou personnalisations réelles si ce n'est pas le cas :

```ABAP
" anti-pattern
DATA(alert_id) = '00000001223678871'.        " this alert really exists
DATA(detection_object_type) = 'FRA_SCLAIM'.  " this detection object type, too
CONSTANTS memory_limit TYPE i VALUE 4096.    " this number looks carefully chosen
```

#### Faites ressortir les différences

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Données de test](#données-de-test) > [Cette section](#faites-ressortir-les-différences)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

Ne forcez pas les lecteurs à comparer de longues chaînes de caractères insignifiantes pour détecter de minuscules différences.

#### Utilisez des constantes pour décrire l'objectif et l'importance des données de test

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Données de test](#données-de-test) > [Cette section](#utilisez-des-constantes-pour-décrire-lobjectif-et-limportance-des-données-de-test)

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

### Assertions

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Cette section](#assertions)

#### Des assertions en faible nombre, ciblées

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#des-assertions-en-faible-nombre-ciblées)

Utilisez des assertions uniquement pour ce à quoi la méthode de test fait référence et limitez-les à un petit nombre.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

L'utilisation excessive d'assertions est un signe indiquant que la méthode n'a pas de cible précise. Cela couple le code productif et le code test dans trop d'emplacements : toute modification d'une fonctionnalité nécessitera alors de réécrire un grand nombre de tests, même s'ils ne sont pas vraiment concernés par cette fonctionnalité modifiée. En outre, le lecteur est dérouté par cette grande variété d'assertions, l'unique assertion importante et distinctive étant "noyée" parmi les autres.

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

#### Utilisez le bon type d'assert

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#utilisez-le-bon-type-dassert)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

Souvent, les asserts sont bien plus utiles qu'il n'y paraît. Par exemple, `assert_equals` inclut la concordance de types et l'indication de descriptions précises si les valeurs diffèrent. L'utilisation d'asserts inappropriés, trop communs, vous entraînera immédiatement de force dans le débogueur, au lieu de vous donner les moyens de voir ce qui est bon/mauvais d'après le message d'erreur.

```ABAP
" anti-pattern
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### Utilisez les asserts pour du contenu, pas pour de la quantité

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#utilisez-les-asserts-pour-du-contenu-pas-pour-de-la-quantité)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

N'écrivez pas des assertions de quantité en nombres magiques si vous pouvez exprimer le contenu réel attendu. Les nombres peuvent varier, même si les attentes sont réalisées. À l'inverse, les nombres peuvent correspondre, même si le contenu est complètement inattendu.

```ABAP
" anti-pattern
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### Utilisez les asserts pour de la qualité, pas pour du contenu

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#utilisez-les-asserts-pour-de-la-qualité-pas-pour-du-contenu)

Si vous vous intéressez à la métaqualité du résultat, et non au contenu réel lui-même, exprimez cela avec un assert adapté :

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

Utiliser des asserts pour le contenu précis a pour effet de masquer ce que vous voulez réellement tester. Cette solution est aussi fragile, car le refactoring peut produire un résultat différent, mais parfaitement acceptable, même si tous vos tests de module trop précis ne fonctionnent plus.

```ABAP
" anti-pattern
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### Utilisez FAIL pour rechercher les exceptions attendues

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#utilisez-fail-pour-rechercher-les-exceptions-attendues)

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

#### Transmettez les exceptions inattendues au lieu d'utiliser catch et fail

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#transmettez-les-exceptions-inattendues-au-lieu-dutiliser-catch-et-fail)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

Votre code test reste axé sur les cas d'utilisation correcte et est, par conséquent, plus facile à lire et à comprendre, comparé à :

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

#### Écrivez des asserts personnalisés pour raccourcir le code et éviter la double saisie

> [Coder proprement avec ABAP](#coder-proprement-avec-abap) > [Table des matières](#table-des-matières) > [Test](#test) > [Assertions](#assertions) > [Cette section](#écrivez-des-asserts-personnalisés-pour-raccourcir-le-code-et-éviter-la-double-saisie)

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

au lieu de copier-coller une partie à répétition.
