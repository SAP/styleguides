# Clean ABAP

> [**Español**](CleanABAP_es.md)
> &nbsp;·&nbsp;
> [English](CleanABAP.md)
> &nbsp;·&nbsp;
> [中文](CleanABAP_zh.md)
> &nbsp;·&nbsp;
> [Français](CleanABAP_fr.md)
> &nbsp;·&nbsp;
> [Deutsch](CleanABAP_de.md)
> &nbsp;·&nbsp;
> [日本語](CleanABAP_ja.md)
> &nbsp;·&nbsp;
> [한국어](CleanABAP_kr.md)
> &nbsp;·&nbsp;
> [Русский](CleanABAP_ru.md)

Esta guía fue adaptada en base a [_Clean Code_ por Robert C. Martin] 
para [ABAP](https://en.wikipedia.org/wiki/ABAP).

La [Cheat Sheet](cheat-sheet/CheatSheet.md) es una versión optimizada para impresión.

[_Clean Code_ por Robert C. Martin]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## Contenido

- [Introducción](#introducción)
  - [Cómo comenzar a programar código limpio](#cómo-comenzar-a-programar-código-limpio)
  - [Cómo refactorizar código legacy](#cómo-refactorizar-código-legacy)
  - [Cómo revisar código automáticamente](#cómo-revisar-código-automáticamente)
  - [Cómo se relaciona con otras guías](#cómo-se-relaciona-con-otras-guías)
  - [Cómo estar en desacuerdo](#cómo-estar-en-desacuerdo)
- [Nomenclatura](#nomenclatura)
  - [Usa nombres descriptivos](#usa-nombres-descriptivos)
  - [Prefiere nombres del dominio de solución o de problema](#prefiere-nombres-del-dominio-de-solución-o-de-problema)
  - [Usa plural](#usa-plural)
  - [Usa nombres pronunciables](#usa-nombres-pronunciables)
  - [Evita abreviaciones](#evita-abreviaciones)
  - [Usa las mismas abreviaciones en todas partes](#usa-las-mismas-abreviaciones-en-todas-partes)
  - [Usa sustantivos para las clases y verbos para los métodos](#usa-sustantivos-para-las-clases-y-verbos-para-los-métodos)
  - [Evita palabras poco específicas como "data", "info", "object"](#evita-palabras-poco-específicas-como-data-info-object)
  - [Elige una palabra por concepto](#elige-una-palabra-por-concepto)
  - [Usa nombres de patrones solo si los estás usando](#usa-nombres-de-patrones-solo-si-los-estás-usando)
  - [Evita codificaciones, como notación Húngara y prefijos](#evita-codificaciones-como-notación-húngara-y-prefijos)
- [Lenguaje](#lenguaje)
  - [Considera el legacy](#considera-el-legacy)
  - [Considera el rendimiento](#considera-el-rendimiento)
  - [Prefiere orientado a objetos que programación procedural](#prefiere-orientado-a-objetos-que-programación-procedural)
  - [Prefiere sentencias funcionales a procedurales](#prefiere-sentencias-funcionales-a-procedurales)
  - [Evita elementos obsoletos del lenguaje](#evita-elementos-obsoletos-del-lenguaje)
  - [Usa patrones de diseño sabiamente](#usa-patrones-de-diseño-sabiamente)
- [Constantes](#constantes)
  - [Usa constantes en lugar de números mágicos](#usa-constantes-en-lugar-de-números-mágicos)
  - [Prefiere clases de enumeración a interfaces de constantes](#prefiere-clases-de-enumeración-a-interfaces-de-constantes)
  - [Si no usas clases de enumeración, agrupa tus constantes](#si-no-usas-clases-de-enumeración-agrupa-tus-constantes)
- [Variables](#variables)
  - [Prefiere declaraciones in-line que al inicio](#prefiere-declaraciones-in-line-que-al-inicio)
  - [No declares variables en ramas opcionales](#no-declares-variables-en-ramas-opcionales)
  - [No encadenes declaraciones](#no-encadenes-declaraciones)
  - [Prefiere REF TO a FIELD-SYMBOL](#prefiere-ref-to-a-field-symbol)
- [Tablas](#tablas)
  - [Usa el tipo correcto de tabla](#usa-el-tipo-correcto-de-tabla)
  - [Evita usar DEFAULT KEY](#evita-usar-default-key)
  - [Prefiere INSERT INTO TABLE a APPEND TO](#prefiere-insert-into-table-a-append-to)
  - [Prefiere LINE_EXISTS a READ TABLE o LOOP AT](#prefiere-line_exists-a-read-table-o-loop-at)
  - [Prefiere READ TABLE a LOOP AT](#prefiere-read-table-a-loop-at)
  - [Prefiere LOOP AT WHERE a IF anidado](#prefiere-loop-at-where-a-if-anidado)
  - [Evita lecturas innecesarias a tablas](#evita-lecturas-innecesarias-a-tablas)
- [Strings](#strings)
  - [Usa ` para definir literales](#usa--para-definir-literales)
  - [Usa | para construir textos](#usa--para-construir-textos)
- [Booleanos](#booleanos)
  - [Usa los booleanos sabiamente](#usa-los-booleanos-sabiamente)
  - [Usa ABAP_BOOL para booleanos](#usa-abap_bool-para-booleanos)
  - [Usa ABAP_TRUE y ABAP_FALSE para hacer comparaciones](#usa-abap_true-y-abap_false-para-hacer-comparaciones)
  - [Usa XSDBOOL para asignar variables booleanas](#usa-xsdbool-para-asignar-variables-booleanas)
- [Condiciones](#condiciones)
  - [Intenta hacer tus condiciones positivas](#intenta-hacer-tus-condiciones-positivas)
  - [Prefiere IS NOT sobre NOT IS](#prefiere-is-not-sobre-not-is)
  - [Considera descomponer condiciones complejas](#considera-descomponer-condiciones-complejas)
  - [Considera extraer condiciones complejas](#considera-extraer-condiciones-complejas)
- [Ifs](#ifs)
  - [No dejes ramas IF vacías](#no-dejes-ramas-if-vacías)
  - [Prefiere CASE a ELSE IF para múltiples condiciones](#prefiere-case-a-else-if-para-múltiples-condiciones)
  - [Mantén la profundidad del anidamiento baja](#mantén-la-profundidad-del-anidamiento-baja)
- [Expresiones regulares](#expresiones-regulares)
  - [Prefiere métodos simples a expresiones regulares](#prefiere-métodos-simples-a-expresiones-regulares)
  - [Prefiere revisiones basis a expresiones regulares](#prefiere-revisiones-basis-a-expresiones-regulares)
  - [Considera construir expresiones regulares complejas](#considera-construir-expresiones-regulares-complejas)
- [Clases](#clases)
  - [Clases: Orientación a objetos](#clases-orientación-a-objetos)
    - [Prefiere objetos a clases estáticas](#prefiere-objetos-a-clases-estáticas)
    - [Prefiere composición a herencia](#prefiere-composición-a-herencia)
    - [No mezcles lógica stateful y stateless en la misma clase](#no-mezcles-lógica-stateful-y-stateless-en-la-misma-clase)
  - [Alcance](#alcance)
    - [Global por default, solo local cuando sea apropiado](#global-por-default-solo-local-cuando-sea-apropiado)
    - [Marcar como FINAL si no fue diseñada para herencia](#marcar-como-final-si-no-fue-diseñada-para-herencia)
    - [Miembros PRIVATE por default, PROTECTED solo si es necesario](#miembros-private-por-default-protected-solo-si-es-necesario)
    - [Considera usar inmutables en lugar de getters](#considera-usar-inmutables-en-lugar-de-getters)
    - [Utiliza READ-ONLY con mesura](#utiliza-read-only-con-mesura)
  - [Constructores](#constructores)
    - [Prefiere NEW a CREATE OBJECT](#prefiere-new-a-create-object)
    - [Si tu clase global es CREATE PRIVATE, deja el constructor público](#si-tu-clase-global-es-create-private-deja-el-constructor-público)
    - [Prefiere múltiples métodos de construcción estáticos a parámetros opcionales](#prefiere-múltiples-métodos-de-construcción-estáticos-a-parámetros-opcionales)
    - [Usa nombres descriptivos para múltiples métodos de construcción](#usa-nombres-descriptivos-para-múltiples-métodos-de-construcción)
    - [Usa singletons únicamente cuando múltiples instancias no hacen sentido](#usa-singletons-únicamente-cuando-múltiples-instancias-no-hacen-sentido)
- [Métodos](#métodos)
  - [Llamadas](#llamadas)
    - [Prefiere llamadas funcionales a procedurales](#prefiere-llamadas-funcionales-a-procedurales)
    - [Omite el uso de RECEIVING](#omite-el-uso-de-receiving)
    - [Omite la palabra clave opcional EXPORTING](#omite-la-palabra-clave-opcional-exporting)
    - [Omite el nombre del parámetro en llamadas de un solo parámetro](#omite-el-nombre-del-parámetro-en-llamadas-de-un-solo-parámetro)
    - [Omite la referencia a sí mismo cuando llames un método de instancia](#omite-la-referencia-a-sí-mismo-cuando-llames-un-método-de-instancia)
  - [Métodos: Orientación a objetos](#métodos-orientación-a-objetos)
    - [Prefiere métodos de instancia a estáticos](#prefiere-métodos-de-instancia-a-estáticos)
    - [Los métodos de instancia públicos deben ser parte de una interfaz](#los-métodos-de-instancia-públicos-deben-ser-parte-de-una-interfaz)
  - [Número de parámetros](#número-de-parámetros)
    - [Procura usar pocos parámetros IMPORTING, menos de tres es lo ideal](#procura-usar-pocos-parámetros-importing-menos-de-tres-es-lo-ideal)
    - [Separa métodos en lugar de agregar parámetros OPTIONAL](#separa-métodos-en-lugar-de-agregar-parámetros-optional)
    - [Usa PREFERRED PARAMETER con mesura](#usa-preferred-parameter-con-mesura)
    - [Usa RETURNING, EXPORTING y CHANGING para exactamente un parámetro](#usa-returning-exporting-y-changing-para-exactamente-un-parámetro)
  - [Tipos de parámetros](#tipos-de-parámetros)
    - [Prefiere RETURNING en lugar de EXPORTING](#prefiere-returning-en-lugar-de-exporting)
    - [No hay problema generalmente con usar RETURNING para tablas grandes](#no-hay-problema-generalmente-con-usar-returning-para-tablas-grandes)
    - [Usa solo RETURNING o EXPORTING, o CHANGING, pero no en conjunto](#usa-solo-returning-o-exporting-o-changing-pero-no-en-conjunto)
    - [Usa CHANGING con mesura, donde aplique](#usa-changing-con-mesura-donde-aplique)
    - [Separa los métodos, en lugar de recibir parámetros booleanos de entrada](#separa-los-métodos-en-lugar-de-recibir-parámetros-booleanos-de-entrada)
  - [Nombres de parámetros](#nombres-de-parámetros)
    - [Considera llamar RESULT al parámetro RETURNING](#considera-llamar-result-al-parámetro-returning)
  - [Inicialización de parámetros](#inicialización-de-parámetros)
    - [Limpia o sobre-escribe parámetros de referencia EXPORTING](#limpia-o-sobre-escribe-parámetros-de-referencia-exporting)
      - [Cuida si la entrada y la salida pueden ser lo mismo](#cuida-si-la-entrada-y-la-salida-pueden-ser-lo-mismo)
    - [No hagas CLEAR a parámetros VALUE](#no-hagas-clear-a-parámetros-value)
  - [Cuerpo del método](#cuerpo-del-método)
    - [Haz una cosa, hazla bien, solo haz eso](#haz-una-cosa-hazla-bien-no-hagas-más-que-eso)
    - [Enfócate en el happy path o en manejo de errores, no en ambos](#enfócate-en-el-happy-path-o-en-manejo-de-errores-no-en-ambos)
    - [Desciende un nivel de abstracción](#desciende-un-nivel-de-abstracción)
    - [Mantén los métodos cortos](#mantén-los-métodos-cortos)
  - [Flujo de control](#flujo-de-control)
    - [Falla rápido](#falla-rápido)
    - [CHECK vs. RETURN](#check-vs-return)
    - [Evita CHECK en otros lugares](#evita-check-en-otros-lugares)
- [Manejo de errores](#manejo-de-errores)
  - [Mensajes](#mensajes)
    - [Haz que los mensajes sean fáciles de encontrar](#haz-que-los-mensajes-sean-fáciles-de-encontrar)
  - [Códigos de retorno](#códigos-de-retorno)
    - [Prefiere excepciones a códigos de retorno](#prefiere-excepciones-a-códigos-de-retorno)
    - [No dejes pasar los errores](#no-dejes-pasar-los-errores)
  - [Excepciones](#excepciones)
    - [Las excepciones son para errores, no para casos regulares](#las-excepciones-son-para-errores-no-para-casos-regulares)
    - [Usa excepciones basadas en clases](#usa-excepciones-basadas-en-clases)
  - [Lanzamiento de excepciones](#lanzamiento-de-excepciones)
    - [Usa tus propias súper clases](#usa-tus-propias-súper-clases)
    - [Lanza un solo tipo de excepción](#lanza-un-solo-tipo-de-excepción)
    - [Usa sub-clases para permitir que el usuario de la clase distinga situaciones de error](#usa-sub-clases-para-permitir-que-el-usuario-de-la-clase-distinga-situaciones-de-error)
    - [Lanza CX_STATIC_CHECK para excepciones que se pueden manejar](#lanza-cx_static_check-para-excepciones-que-se-pueden-manejar)
    - [Lanza CX_NO_CHECK para situaciones de las que típicamente no se puede recuperar](#lanza-cx_no_check-para-situaciones-de-las-que-típicamente-no-se-puede-recuperar)
    - [Considera CX_DYNAMIC_CHECK para excepciones que no se pueden evitar](#considera-cx_dynamic_check-para-excepciones-que-no-se-pueden-evitar)
    - [Lanza un dump para situaciones que son completamente irrecuperables](#lanza-un-dump-para-situaciones-que-son-completamente-irrecuperables)
    - [Prefiere RAISE EXCEPTION NEW en lugar de RAISE EXCEPTION TYPE](#prefiere-raise-exception-new-en-lugar-de-raise-exception-type)
  - [Atrapando excepciones](#atrapando-excepciones)
    - [Envuelve excepciones foráneas en lugar de permitir que invadan tu código](#envuelve-excepciones-foráneas-en-lugar-de-permitir-que-invadan-tu-código)
- [Comentarios](#comentarios)
  - [Exprésate en código, no en comentarios](#exprésate-en-código-no-en-comentarios)
  - [Los comentarios no son excusa para nombrar mal objetos](#los-comentarios-no-son-excusa-para-nombrar-mal-objetos)
  - [Usa métodos en lugar de comentarios para segmentar tu código](#usa-métodos-en-lugar-de-comentarios-para-segmentar-tu-código)
  - [Escribe comentarios para explicar el por qué, no el qué](#escribe-comentarios-para-explicar-el-por-qué-no-el-qué)
  - [El diseño va en los documentos de diseño, no en el código](#el-diseño-va-en-los-documentos-de-diseño-no-en-el-código)
  - [Usa " para comentar, no *](#usa--para-comentar-no-)
  - [Usa comentarios antes de la sentencia a la que hacen referencia](#usa-comentarios-antes-de-la-sentencia-a-la-que-hacen-referencia)
  - [Borra el código en lugar de comentarlo](#borra-el-código-en-lugar-de-comentarlo)
  - [Usa FIXME, TODO y XXX y agrega tu usuario](#usa-fixme-todo-y-xxx-y-agrega-tu-usuario)
  - [No agregues prototipos ni comentarios de fin de métodos](#no-agregues-prototipos-ni-comentarios-de-fin-de-métodos)
  - [No repitas los mensajes o textos en comentarios](#no-repitas-los-mensajes-o-textos-en-comentarios)
  - [Usa ABAP Doc únicamente para APIs públicas](#usa-abap-doc-únicamente-para-apis-públicas)
  - [Usa pragmas en lugar de pseudo-comentarios](#usa-pragmas-en-lugar-de-pseudo-comentarios)
- [Formato](#formato)
  - [Sé consistente](#sé-consistente)
  - [Optimiza para lectura, no para escritura](#optimiza-para-lectura-no-para-escritura)
  - [Usa el Pretty Printer antes de activar](#usa-el-pretty-printer-antes-de-activar)
  - [Usa la configuración de Pretty Printer de tu equipo](#usa-la-configuración-de-pretty-printer-de-tu-equipo)
  - [No más de una sentencia por línea de código](#no-más-de-una-sentencia-por-línea-de-código)
  - [Mantén una longitud de línea razonable](#mantén-una-longitud-de-línea-razonable)
  - [Condensa tu código](#condensa-tu-código)
  - [Usa una línea en blanco para separar cosas, pero no más](#usa-una-línea-en-blanco-para-separar-cosas-pero-no-más)
  - [No te obsesiones con separar usando líneas en blanco](#no-te-obsesiones-con-separar-usando-líneas-en-blanco)
  - [Alinea asignaciones al mismo objeto, pero no a objetos diferentes](#alinea-asignaciones-al-mismo-objeto-pero-no-a-objetos-diferentes)
  - [Cierra paréntesis en la última línea de código](#cierra-paréntesis-en-la-última-línea-de-código)
  - [Mantén llamadas de un solo parámetro en una línea](#mantén-llamadas-de-un-solo-parámetro-en-una-línea)
  - [Mantén los parámetros detrás de la llamada](#mantén-los-parámetros-detrás-de-la-llamada)
  - [Si haces un salto de línea, indenta parámetros debajo de la llamada](#si-haces-un-salto-de-línea-indenta-parámetros-debajo-de-la-llamada)
  - [Usa saltos de línea para múltiples parámetros](#usa-saltos-de-línea-para-múltiples-parámetros)
  - [Alinea los parámetros](#alinea-los-parámetros)
  - [Usa un salto de línea si la llamada a un método se vuelve muy extensa](#usa-un-salto-de-línea-si-la-llamada-a-un-método-se-vuelve-muy-extensa)
  - [Usa indentado apropiado](#usa-indentado-apropiado)
  - [Indenta declaraciones in-line como llamadas a métodos](#indenta-declaraciones-in-line-como-llamadas-a-métodos)
  - [No alinees los TYPE](#no-alinees-los-type)
- [Testing](#testing)
  - [Principios](#principios)
    - [Escribe código que se pueda probar](#escribe-código-que-se-pueda-probar)
    - [Permite que otros hagan mock de tu código](#permite-que-otros-hagan-mock-de-tu-código)
    - [Reglas de legibilidad](#reglas-de-legibilidad)
    - [No hagas copias ni escribas reportes de prueba](#no-hagas-copias-ni-escribas-reportes-de-prueba)
    - [Prueba componentes públicos, no los privados](#prueba-componentes-públicos-no-los-privados)
    - [No te obsesiones con la cobertura del código](#no-te-obsesiones-con-la-cobertura-del-código)
  - [Clases de prueba](#clases-de-prueba)
    - [Llama las clases locales de prueba de acuerdo a su objetivo](#llama-las-clases-locales-de-prueba-de-acuerdo-a-su-objetivo)
    - [Coloca tus pruebas en clases locales](#coloca-tus-pruebas-en-clases-locales)
    - [Coloca métodos de ayuda en clases de ayuda](#coloca-métodos-de-ayuda-en-clases-de-ayuda)
    - [Cómo ejecutar clases de prueba](#cómo-ejecutar-clases-de-prueba)
  - [Código bajo prueba](#código-bajo-prueba)
    - [Nombra el código bajo prueba con sentido, o usa CUT como default](#nombra-el-código-bajo-prueba-con-sentido-o-usa-cut-como-default)
    - [Prueba sobre interfaces, no implementaciones](#prueba-sobre-interfaces-no-implementaciones)
    - [Extrae la llamada al código bajo prueba a su propio método](#extrae-la-llamada-al-código-bajo-prueba-a-su-propio-método)
  - [Inyección](#inyección)
    - [Usa inversión de dependencias para inyectar dobles de prueba](#usa-inversión-de-dependencias-para-inyectar-dobles-de-prueba)
    - [Considera usar la herramienta de ABAP test double](#considera-usar-la-herramienta-de-abap-test-double)
    - [Explota el uso de las herramientas de prueba](#explota-el-uso-de-las-herramientas-de-prueba)
    - [Usa test seams como una solución temporal](#usa-test-seams-como-una-solución-temporal)
    - [Usa LOCAL FRIENDS para acceder al constructor inversor de dependencias](#usa-local-friends-para-acceder-al-constructor-inversor-de-dependencias)
    - [No uses LOCAL FRIENDS para invadir el código bajo prueba](#no-uses-local-friends-para-invadir-el-código-bajo-prueba)
    - [No cambies el código productivo para poder probarlo](#no-cambies-el-código-productivo-para-poder-probarlo)
    - [No uses herencia para hacer mock a métodos](#no-uses-herencia-para-hacer-mock-a-métodos)
    - [No hagas mock sin necesidad](#no-hagas-mock-sin-necesidad)
    - [No crees librerías para pruebas](#no-crees-librerías-para-pruebas)
  - [Métodos de prueba](#métodos-de-prueba)
    - [Nomenclatura para métodos de prueba: considera el supuesto y el resultado esperado](#nomenclatura-para-métodos-de-prueba-considera-el-supuesto-y-el-resultado-esperado)
    - [Usa dado-cuando-entonces](#usa-dado-cuando-entonces)
    - ["Cuando" es exactamente una llamada](#cuando-es-exactamente-una-llamada)
    - [No uses el método TEARDOWN a menos que realmente lo necesites](#no-uses-el-método-teardown-a-menos-que-realmente-lo-necesites)
  - [Datos de prueba](#datos-de-prueba)
    - [Haz que sea fácil detectar la intención](#haz-que-sea-fácil-detectar-la-intención)
    - [Haz que sea fácil detectar las diferencias](#haz-que-sea-fácil-detectar-las-diferencias)
    - [Usa constantes para describir el objetivo y la importancia de los datos de prueba](#usa-constantes-para-describir-el-objetivo-y-la-importancia-de-los-datos-de-prueba)
  - [Aserciones](#aserciones)
    - [Usa pocas aserciones, enfocadas](#usa-pocas-aserciones-enfocadas)
    - [Usa el tipo de aserción correcto](#usa-el-tipo-de-aserción-correcto)
    - [Haz aserciones sobre el contenido, no la cantidad](#haz-aserciones-sobre-el-contenido-no-la-cantidad)
    - [Haz aserciones sobre la calidad, no el contenido](#haz-aserciones-sobre-la-calidad-no-el-contenido)
    - [Usa FAIL para evaluar excepciones esperadas](#usa-fail-para-evaluar-excepciones-esperadas)
    - [Propaga excepciones inesperadas en lugar de atraparlas y enviar error](#propaga-excepciones-inesperadas-en-lugar-de-atraparlas-y-enviar-error)
    - [Escribe aserciones a la medida para reducir el código y evitar duplicados](#escribe-aserciones-a-la-medida-para-reducir-el-código-y-evitar-duplicados)

## Introducción

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#introducción)

### Cómo comenzar a programar código limpio

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción) > [Esta sección](#cómo-comenzar-a-programar-código-limpio)

Si eres nuevo al Código Limpio, deberías leer primero
[ _Código Limpio_] (o Clean Code) por Robert C. Martin.
La [iniciativa Clean Code Developer](https://clean-code-developer.com/)
puede ayudarte a comenzar con una introducción didáctica al tema en general.

Te recomendamos comenzar con cosas que sean fáciles de entender y ampliamente aceptadas,
como [Booleanos](#booleanos), [Condiciones](#condiciones) e [Ifs](#ifs).

Probablemente te beneficiarás más de la sección [Métodos](#métodos),
especialmente [Haz una cosa, hazla bien, solo haz eso](#haz-una-cosa-hazla-bien-solo-haz-eso) y [Mantén los métodos cortos](#mantén-los-métodos-cortos), ya que mejoran tremendamente la estructura de tu código.

Algunos temas de esta guía pueden iniciar conversaciones difíciles en los equipos
que tienen experiencia en lo que hacen, pero son nuevos al código limpio.
Estos temas son perfectamente "sanos", pero la gente puede tener problemas
adaptándose a ellos en el principio.

Una vez que domines los primeros temas, pasa a otros más controversiales;
ya que especialmente [Comentarios](#comentarios), [Nomenclatura](#nomenclatura), y [Formato](#formato)
pueden llevar a discusiones casi religiosas
y solo deberían ser utilizadas por equipos que ya tengan prueba de los efectos positivos del código limpio.

### Cómo refactorizar código legacy

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción) > [Esta sección](#cómo-refactorizar-código-legacy)

Los temas [Booleanos](#booleanos), [Condiciones](#condiciones), [Ifs](#ifs),
y [Métodos](#métodos) son los que más te recompensan al trabajar con un proyecto legacy con mucho código
que no puedes o no quieres cambiar, dado que puede ser aplicado solo al código nuevo sin conflicto.

El tema [Nomenclatura](#nomenclatura) es muy exigente para proyectos legacy,
ya que puede introducir una fuerte diferencia entre código viejo y nuevo,
al grado que secciones como [Evita codificaciones, como notación Húngara y prefijos](#evita-codificaciones-como-notación-húngara-y-prefijos) es mejor ignorarlas.

Trata de no mezclar estilos diferentes de programación dentro del mismo objeto de desarrollo 
al llevar a cabo refactorización. Si el código legacy contiene solo declaraciones al inicio
y una refactorización completa en declaraciones in-line no es posible, es mejor mantener 
el estilo del código legacy que mezclar ambos estilos. Hay varias situaciones similares
donde mezclar estilos puede causar confusión, por ejemplo:

- Mezclar `REF TO` y `FIELD-SYMBOL` al hacer iteraciones.
- Mezclar `NEW` y `CREATE OBJECT` al llamar un `CONSTRUCTOR`.
- Mezclar `RETURNING` y `EXPORTING` en los prototipos que solo regresan / exportan un parámetro.

Hemos observado buenos resultados con un plan de cuatro pasos para refactorización:

1. Convence al equipo. Comunica y explica el nuevo estilo y logra que todos en el equipo
lo acepten. No necesitas comprometerte con toda la guía desde el inicio, solo comienza
con un sub-conjunto de reglas indiscutibles y evoluciona a partir de eso. 

2. Sigue la regla de los _boy scout_ en tu rutina de trabajo diaria:
_siempre deja el código un poco más limpio de lo que lo encontraste_.
No te obsesiones dedicando horas a "limpiar el campamento", solo
dedica un par de minutos adicionales y observa como las mejoras se acumulan con el tiempo.

3. Construye _islas limpias_: de vez en cuando, elige un pequeño objeto o componente y trata
de hacerlo limpio en todos los sentidos. Estas islas demuestran el beneficio de lo que estás
haciendo y forman una base sólidamente probada para hacer más refactorizaciones en el futuro.

4. Habla de ello. No importa si usas [Revisiones de código Fagan](https://en.wikipedia.org/wiki/Fagan_inspection) como antaño, llevas a cabo sesiones informativas o formas paneles de discusión en tu herramienta de chat favorita: 
vas a necesitar hablar de tus experiencias y aprendizajes, para habilitar que el equipo llegue
a un entendimiento común.

### Cómo revisar automáticamente

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción) > [Esta sección](#cómo-revisar-código-automáticamente)

[code pal para ABAP](https://github.com/SAP/code-pal-for-abap)
provee una suite comprensiva de revisiones automáticas para Clean ABAP.

ABAP Test Cockpit, Code Inspector, Extended Check y Checkman proveen algunas revisiones
que te pueden ayudar a detectar ciertos problemas.

[abapOpenChecks](https://github.com/larshp/abapOpenChecks),
es una colección Open Source de revisiones de Code Inspector,
también cubre algunos anti-patrones.

[abaplint](https://github.com/abaplint/abaplint) es una reimplementación Open Source del parser de ABAP. Funciona
sin un sistema SAP y está pensado para ser utilizado de manera serializada junto con abapGit. Ofrece múltiples integraciones (GitHub Actions, Jenkins, editores de texto...), cubre algunos de los anti-patrones y puede ser usado también para revisar formato y convenciones de código.

### Cómo se relaciona con otras guías

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción)> [Esta sección](#cómo-se-relaciona-con-otras-guías)

Nuestra guía sigue el _espiritú_ de Clean Codem
lo que significa que hicimos algunos ajustes para el lenguaje de programación ABAP,
por ejemplo [Lanza CX_STATIC_CHECK para excepciones que se pueden manejar](#lanza-cx_static_check-para-excepciones-que-se-pueden-manejar).

Algunos hechos provienen de la [Guía para programación ABAP](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm),
con la cual esta guía es compatible en su mayor parte. Las desviaciones respecto a esa guía siempre están indicadas
y son siempre con el objetivo de tener código más limpio.

Esta guía también respeta las 
[Recomendaciones de DSAG para desarrollo ABAP](https://www.dsag.de/sites/default/files/2020-12/dsag_recommendation_abap_development.pdf),
aunque es más precisa en la mayoría de los casos.

Desde su publicación, Clean ABAP se ha convertido en una guía de referencia para muchos
de los equipos de desarrollo de SAP in-house, incluyendo los cientos de desarrolladores
que trabajan en S/4HANA.

### Cómo estar en desacuerdo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción) > [Esta sección](#cómo-estar-en-desacuerdo)

Escribimos esta guía de estilo para lectores que ya están familiarizados con Clean Code o que están en el proceso de hacerlo, con un enfoque específico en cómo aplicar código limpio _específicamente para ABAP_.

Por este motivo, por favor ten en cuenta que no introducimos todos los conceptos con el mismo detalle
que el libro original y las fuentes mencionadas: estas fuentes aún vale la pena que sean leídas, especialmente
si no estás de acuerdo con elementos de esta guía porque no los explicamos tan bien.
Usa los enlaces en las secciones para leer el trasfondo de nuestra guía.

Eres libre de discutir y estar en desacuerdo con cualquier cosa que decimos aquí.
Uno de los pilares de Clean Code es que _el equipo manda_. Solo asegúrate de 
darle a cada cosa una oportunidad justa antes de descartarla.

[CONTRIBUTING.md](../CONTRIBUTING.md) sugiere maneras en que puedes cambiar esta guía o desviarte de ella en detalles menores. Toda contribución o discusión se lleva a cabo en inglés.

## Nomenclatura

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#nomenclatura)

### Usa nombres descriptivos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#use-descriptive-names)

Usa nombres que muestren el contenido y significado de las cosas.

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

No te enfoques en el tipo de datos o codificación técnica.
Rara vez contribuyen a entender el código.

```ABAP
" anti-pattern
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[No trates de arreglar nombres malos con comentarios.](#los-comentarios-no-son-excusa-para-nombrar-mal-objetos)

> Lee más en el _Capítulo 2: Nombres con sentido: Usar nombres que revelen las intenciones_ de [_Código Limpio_ por Robert C. Martin].

### Prefiere nombres del dominio de solución o de problema

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#prefiere-nombres-del-dominio-de-solución-o-de-problema)

Busca nombres buenos en el dominio de solución, por ejemplo en ciencias de la computación términos como
"pila" o "árbol"; y en el dominio de problema, por ejemplo "cuenta" o "libro_mayor".

Las capas que pertenecen a negocio cuando son nombradas de acuerdo al dominio de problema.
Esto es especialmente cierto para componentes que son diseñados bajo el diseño guiado por el dominio, como las APIs
y los objetos de negocio.

Las capas que proveen la mayor parte de la funcionalidad técnica, como las clases fábrica y algoritmos abstractos, 
sonarán mejor cuando son nombradas de acuerdo al dominio de solución.

En cualquier caso, no trates de hacer tu propio lenguaje. Necesitamos poder intercambiar información
entre desarrolladores, product owners y clientes, así que elige nombres que todos entiendan
sin requerir un diccionario a la medida.

> Lee más en _Capítulo 2: Nombres con sentido: Usar nombres de dominios de soluciones_ y _[...]:
> Usar nombres de dominios de problemas_ de [_Código Limpio_ por Robert C. Martin].

### Usa plural

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-plural)

Hay una práctica antigua en SAP en la que nombran las tablas de las entidades en singular,
por ejemplo `country` para una "tabla de países".
Una tendencia común en el mundo fuera de SAP es usar plural para listas de cosas.
Por lo tanto, recomendamos elegir `countries` en su lugar.

> Este consejo es principalmente para cosas como variables y propiedades. 
> Para objetos de desarrollo, puede haber patrones
> que también tienen sentido, por ejemplo la convención ampliamente usada
> de nombrar tablas de bases de datos ("tablas transparentes") en singular.

> Lee más en _Capítulo 2: Nombres con sentido: Usar nombres que revelen las intenciones_ de [_Código Limpio_ por Robert C. Martin].

### Usa nombres pronunciables

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-nombres-pronunciables)

Pensamos y hablamos mucho de objetos, así que usa nombres que puedas pronunciar,
por ejemplo, es preferible `tipos_de_objeto_de_detección` a algo ininteligible como `tipobjdet`

> Lee más en _Capítulo 2: Nombres con sentido: Usar nombres que se puedan pronunciar_ de [_Código Limpio_ por Robert C. Martin]

### Evita abreviaciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#evita-abreviaciones)

Si tienes suficiente espacio, escribe los nombres completos.
Comienza a abreviar únicamente si excedes el límite de caracteres.

Si tienes que abreviar, comienza con las palabras con _poca importancia_.

Abreviar puede parece eficiente inicialmente, pero se vuelve ambiguo muy rápido.
Por ejemplo, nombrar una variable como `cust` significa "customizing", "customer" o "custom"?
Las tres son muy comunes en aplicaciones SAP.

> Lee más en _Capítulo 2: Nombres con sentido: Realizar distinciones con sentido_ de [_Código Limpio_ por Robert C. Martin].

### Usa las mismas abreviaciones en todas partes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-las-mismas-abreviaciones-en-todas-partes)

Las personas realizan búsquedas usando palabras clave para encontrar código relevante.
Apoya esto usando la misma abreviación para el mismo concepto.
Por ejemplo, siempre abrevia "tipo de objeto de detección" a "tipobjdet",
en lugar de mezclar "tod", "tipod", "tipobjd", etc.

> Lee más en _Capítulo 2: Nombres con sentido: Usar nombres que se puedan buscar_ de [_Código Limpio_ por Robert C. Martin].

### Usa sustantivos para las clases y verbos para los métodos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-sustantivos-para-las-clases-y-verbos-para-los-métodos)

Usa sustantivos o frases con sustantivos para nombrar clases, interfaces y objetos:

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

Usa verbos o frases de verbos para nombrar métodos:

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```
Iniciar métodos booleanos con verbos como `is_` y `has` provee un flujo de lectura agradable:

```ABAP
IF is_empty( table ).
```
Recomendamos nombrar las funciones como a los métodos:

```ABAP
FUNCTION /clean/read_alerts
```

### Evita palabras poco específicas como "data", "info", "object"

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#evita-palabras-poco-específicas-como-data-info-object)

Omite palabras que generan ruido:

```ABAP
account  " instead of account_data
alert    " instead of alert_object
```
o reemplazalas con algo específico que realmente agregue valor

```ABAP
user_preferences          " instead of user_info
response_time_in_seconds  " instead of response_time_variable
```

> Lee más en _Capítulo 2: Nombres con sentido: Realizar distinciones con sentido_ de [_Código Limpio_ por Robert C. Martin]

### Elige una palabra por concepto

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#elige-una-palabra-por-concepto)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

Elige un término para un concepto y apégate a él; no lo mezcles usando otros sinónimos.
Los sinónimos harán al lector perder el tiempo buscando una diferencia que no está ahí.

```ABAP
" anti-pattern
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> Lee más en _Capítulo 2: Nombres con sentido: Una palabra por concepto_ de [_Código Limpio_ por Robert C. Martin]

### Usa nombres de patrones solo si los estás usando

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-nombres-de-patrones-solo-si-los-estás-usando)

No uses los nombres de patrones de diseño de software para clases e interfaces a menos que de 
verdad los estés usando. Por ejemplo, no llames a tu clase `file_factory` a menos que realmente
implemente el patrón de diseño de fábrica.
Los patrones más comunes son:
[singleton](https://en.wikipedia.org/wiki/Singleton_pattern),
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern),
[facade](https://en.wikipedia.org/wiki/Facade_pattern),
[composite](https://en.wikipedia.org/wiki/Composite_pattern),
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern),
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern),
[observer](https://en.wikipedia.org/wiki/Observer_pattern), y
[strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

> Lee más en _Capítulo 2: Nombres con sentido: Evitar la desinformación_ de [_Código Limpio_ por Robert C. Martin]

### Evita codificaciones, como notación Húngara y prefijos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#avoid-codificaciones-esp-hungarian-notation-and-prefixes)

Te sugerimos que te deshagas de _todas_ las codificaciones con prefijos.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

en lugar del innecesariamente largo

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> [Evita codificaciones](sub-sections/AvoidEncodings.md)
> describe el razonamiento a detalle.

## Lenguaje

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#lenguaje)

### Considera el legacy

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Lenguaje](#lenguaje) > [Esta sección](#considera-el-legacy)

Si programas para versiones anteriores de ABAP, toma el consejo de esta guía con cuidado:
Varias recomendaciones hacen uso de síntaxis relativamente nueva que podría no estar
soportada en versiones anteriores de ABAP.
Verifica que la guía que quieres seguir es viable en la versión más vieja que tienes que soportar.
No descartes Código Limpio completamente - la gran mayoría de las reglas (ej. nomenclatura, comentarios)
funcionarán en _cualquier_ versión de ABAP.

### Considera el rendimiento

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Lenguaje](#lenguaje) > [Esta sección](#considera-el-rendimiento)

Si programas componentes de alto rendimiento, toma el consejo de esta guía con cuidado:
Algunos aspectos de Código Limpio pueden hacer las cosas más lentas (más llamadas a métodos)
o consumir más memoria (más objetos).
ABAP tiene algunas particularidades que pueden intensificar este detalle, por ejemplo
compara los tipos de dato al llamar un método, por lo que al dividir un método largo
en varios sub-métodos puede hacer el código más lento.

Sin embargo, recomendamos fuertemente no optimizar prematuramente, basándose en miedos irracionales.
La gran mayoría de las reglas (ej. nomenclatura, comentarios) no tiene impacto negativo.
Trata de construir las cosas de una manera limpia, orientada a objetos.
Si algo es muy lento, haz una medición de rendimiento.
Solo entonces deberías tomar una decisión, basada en hechos, para descartar reglas.

Algunos pensamientos adicionales, tomados en parte del Capítulo 2 de [_Refactoring_ por Martin Fowler](https://martinfowler.com/books/refactoring.html):

En una aplicación típica la mayoría del tiempo de ejecución se concentra en una proporción muy
pequeña del código. Tan poco como 10% del código puede consumir el 90% del tiempo de ejecución
y especialmente en ABAP una proporción grande del tiempo de ejecución se suele
concentrar en bases de datos.

Por lo tanto, no es el mejor uso de los recursos dedicar esfuerzo significativo en tratar
de hacer _todo_ el código súper eficiente todo el tiempo. No estamos sugiriendo que ignores
el rendimiento, pero trata de enfocarte en código más limpio y estructurado durante el 
desarrollo inicial y usa el profiler de ABAP para identificar áreas críticas para optimizar.

De hecho, argumentamos que una estrategia como esta tiene un efecto neto positivo en el rendimiento
ya que es un esfuerzo enfocado de optimización y debería ser más fácil detectar cuellos de botella, 
refactorizar y afinar código bien estructurado.

### Prefiere orientado a objetos que programación procedural

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Lenguaje](#lenguaje) > [Esta sección](#prefiere-orientado-a-objetos-que-programación-procedural)

Los programas orientados a objetos (clases, interfaces) están mejor segmentados
y pueden ser refactorizados y probados más fácilmente que el código procedural (funciones, programas).
Aunque hay situaciones donde debes usar objetos procedurales (una función para una RFC, un programa
para una transacción), estos objetos deberían hcaer poco menos que llamar la clase correspondiente
que contiene la funcionalidad:

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> [Grupos de funciones vs. Clases](sub-sections/FunctionGroupsVsClasses.md)
> describe la diferencia a detalle.

### Prefiere sentencias funcionales a procedurales

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Lenguaje](#lenguaje) > [Esta sección](#prefiere-sentencias-funcionales-a-procedurales)

Son usualmente más cortas y son más naturales para los programadores modernos.

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

DATA(line) = value_pairs[ name = 'A' ]. " entry must exist
DATA(line) = VALUE #( value_pairs[ name = 'A' ] OPTIONAL ). " entry can be missing
" READ TABLE value_pairs INTO DATA(line) WITH KEY name = 'A'.

DATA(exists) = xsdbool( line_exists( value_pairs[ name = 'A' ] ) ).
IF line_exists( value_pairs[ name = 'A' ] ).
" READ TABLE value_pairs TRANSPORTING NO FIELDS WITH KEY name = 'A'.
" DATA(exists) = xsdbool( sy-subrc = 0 ).
```

Varias de las reglas detalladas más adelante son reiteraciones específicas de este mismo punto.

### Evita elementos obsoletos del lenguaje

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Lenguaje](#lenguaje) > [Esta sección](#evita-elementos-obsoletos-del-lenguaje)

Cuando actualices tu versión de ABAP,
asegúrate de revisar elementos obsoletos del lenguaje
y evita utilizarlos.

Por ejemplo, las variables de entorno se escapan con `@`, 
lo cual hace más claro qué es una variable del programa y
qué es una columna en la base de datos,

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

comparada con la versión [obsoleta sin escapar](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm)

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

Nuevas alternativas tienden a mejorar la legibilidad del código
y reducir conflictos de diseño con paradigmas modernos de programación,
de manera que cambiar a ellas pueden automáticamente resultar
en código más limpio.

Aunque continuan funcionando, elementos obsoletos pueden dejar
de beneficiarse de optimizaciones en términos de velocidad de procesamiento
y consumo de memoria.

Con elementos modernos del lenguaje, puedes entrenar más fácilmente a los ABAPers más jóvenes
quienes no estarán familiarizados con las sentencias obsoletas
ya que ya no se enseñan en los entrenamientos de SAP.

La documentación de SAP NetWeaver contiene una sección actualizada que enlista elementos
obsoletos del lenguaje:
[NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm).

### Usa patrones de diseño sabiamente

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Lenguaje](#lenguaje) > [Esta sección](#usa-patrones-de-diseño-sabiamente)

Donde sean apropiados y provean un beneficio tangible.
No apliques patrones de diseño en todas partes solo por hacerlo.

## Constantes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#constantes)

### Usa constantes en lugar de números mágicos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Constantes](#constantes) > [Esta sección](#usa-constantes-en-lugar-de-números-mágicos)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

es más claro que

```ABAP
" anti-pattern
IF abap_type = 'D'.
```

> Lee más en _Capítulo 17: Síntomas y heurística: G25:
> Sustituir números mágicos por constantes con nombre_ de [_Código Limpio_ por Robert C. Martin].

### Prefiere clases de enumeración a interfaces de constantes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Constantes](#constantes) > [Esta sección](#prefiere-clases-de-enumeración-a-interfaces-de-constantes)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

o

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```
en lugar de mezclar conceptos no relacionados
o llevar erróneamente a la gente a la conclusión de
que las colecciones de constantes pueden ser "implementadas":

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

> [Enumeraciones](sub-sections/Enumerations.md)
> describe patrones comunes de enumeración
> y discute sus ventajas y desventajas.
>
> Lee más en _Capítulo 17: Síntomas y heurística: J3: Constantes frente a enumeraciones_ de [_Código Limpio_ por Robert C. Martin].

### Si no usas clases de enumeración, agrupa tus constantes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Constantes](#constantes) > [Esta sección](#si-no-usas-clases-de-enumeración-agrupa-tus-constantes)

Si juntas constantes de una manera desordenada, por ejemplo en una interfaz, agrúpalas:

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

Hace la relación más clara que:

```ABAP
" Anti-pattern
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

El grupo también te permite acceder a ellas en grupo, por ejemplo para validación de entradas:

```ABAP
DO.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(<constant>).
  IF sy-subrc IS INITIAL.
    IF input = <constant>.
      DATA(is_valid) = abap_true.
      RETURN.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.
ENDDO.
```

> Lee más en _Capítulo 17: Síntomas y heurística: G27: Estructura sobre convención_ de [_Código Limpio_ por Robert C. Martin].

## Variables

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#variables)

### Prefiere declaraciones in-line que al inicio

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Variables](#variables) > [Esta sección](#prefiere-declaraciones-in-line-que-al-inicio)

Si sigues esta guía, tus métodos se volverán tan cortos (3-5 sentencias)
que declarar variables in-line será más natural

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

que declarar variables con una sección separada de `DATA` al inicio del método

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

> Lee más en _Capítulo 5: Formato vertical: Distancia Vertical: Declaraciones de variables_ de [_Código Limpio_ por Robert C. Martin].

### No declares variables en ramas opcionales

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Variables](#variables) > [Esta sección](#no-declares-variables-en-ramas-opcionales)

```ABAP
" anti-pattern
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```
Esto funciona bien porque ABAP maneja declaraciones in-line como si estuvieran al inicio del método.
Sin embargo, es extremadamente confuso para los lectores, especialmente si el método es largo y no
detectas la declaración inmediatamente.
En este caso, no uses in-line y coloca la declaración al inicio.

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> Lee más en _Capítulo 5: Formato vertical: Distancia Vertical: Declaraciones de variables_ de [_Código Limpio_ por Robert C. Martin].

### No encadenes declaraciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Variables](#variables) > [Esta sección](#no-encadenes-declaraciones)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO reader.
```
El encadenamiento sugiere que las variables definidas están relacionadas en un nivel lógico.
Para usarlo consistentemente, tienes que asegurarte de que todas las variables encadenadas
pertenecen juntas lógicamente e introducir grupos encadenados adicionales para agregar
variables.
Aunque esto es posible, el esfuerzo rara vez vale la pena.

El encadenamiento complica innecesariamente reformatear y refactorizar el código,
ya que cada línea se ve diferente y cambiarlas requiere lidiar con 
comas, puntos, dos puntos, lo cual no vale la pena.

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO reader.
```

> También lee [No alinees los TYPE](#no-alinees-los-type)  
> Si el encadenamiento de la declaración de datos es usada, entonces usa una cadena por cada grupo de variables que pertenecen juntas lógicamente.

### Prefiere REF TO a FIELD-SYMBOL

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Variables](#variables) > [Esta sección](#prefiere-ref-to-a-field-symbol)

> Esta sección [está siendo discutida](https://github.com/SAP/styleguides/issues/115).
> `FIELD-SYMBOL` parece ser considerablemente más rápida
> al iterar sobre tablas internas,
> tanto que la recomendación de usar `REF TO`
> para estos casos puede empeorar el rendimiento.

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

en lugar del equivalente

```ABAP
" anti-pattern
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

excepto cuando se necesiten los `field-symbols`

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

Las revisiones de código demuestran que la gente tiende a escoger entre las dos arbitrariamente,
"solo porque sí", "porque siempre hacemos LOOPs de esa manera", o "sin una razón en particular".
Las decisiones arbitrarias hacen que el lector pierda el tiempo en la pregunta sin sentido de por qué
se usó uno en lugar del otro, por lo tanto se debería reemplazar con decisiones precisas y bien fundamentadas.
Nuestra recomendación está basada en el siguiente razonamiento:

- Los `field-symbols` pueden hacer cosas que las referencias no pueden, como acceder dinámicamente a los componentes de una estructura.
De la misma manera, las referencias pueden hacer cosas que los `field-symbols` no, como construir dinámicamente una estructura de datos con tipos. En resumen, elegir una sola para todas las situaciones no es posible.

- En ABAP orientado a objetos, las referencias se usan por todas partes y no se pueden evitar, ya que cualquier
objeto es una `REF TO <class-name>`. 
En contraste, los `field-symbols` son solamente requeridos estrictamente en situaciones especiales al usar
programación dinámica. Las referencias por lo tanto tienen una preferencia natural en cualquier programa
orientado a objetos.

- Los `field-symbols` son más cortos que las referencias, pero el ahorro resultante en memoria es tan
pequeño que puede ser ignorado sin problema.
De la misma manera, la velocidad no es un problema. Como consecuencia, no hay razón relacionada con el rendimiento
para preferir uno sobre el otro.

> Lee más en el artículo
> [_Accessing Data Objects Dynamically_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm).

## Tablas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#tablas)

### Usa el tipo correcto de tabla

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#usa-el-tipo-correcto-de-tabla)

- Típicamente se usan tablas del tipo `HASHED` para **tablas grandes** que son 
**llenadas en un solo paso**, **nunca se modifican** y son **leídas seguido por su llave**.
Su sobrecoste en memoria y procesamiento hace a las tablas hash únicamente valiosas
cuando se tienen cantidades grandes de datos y muchos accesos de lectura.
Cada cambio al contenido de la tabla requiere costosos recálculos del hash,
así que no uses este tipo para tablas que son modificadas muy frecuentemente.

- Típicamente se usan tablas del tipo `SORTED` para **tablas grandes**
que necesitan estar **ordenadas en todo momento**, que son **llenadas poco a poco** o que 
**necesitan ser modificadas** y **leídas por una o más claves parciales o completas** o procesadas
**en cierto orden**.
Agregar, cambiar o quitar contenido require encontrar el punto de inserción adecuado,
pero no requiere reajustar los índices de la tabla completa.
Las tablas ordenadas demustran su valor únicamente para cantidades grandes de accesos de lectura.

- Usa tablas del tipo `STANDARD` para **tablas pequeñas**, donde el indexado genera más coste
que beneficio, y **arreglos**, donde no te interesa el orden de las filas o quieres procesarlas
exactamente en el orden que se agregaron a la tabla. Además, si se requieren diferentes tipos
de acceso a la tabla (por ejemplo, acceso por índice y acceso ordenado via `SORT` y `BINARY SEARCH`)

> Ésta es una guía a grandes rasgos.
> Encuentra más detalles en [_Selection of Table Category_ in the ABAP Language Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm).

### Evita usar DEFAULT KEY

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#evita-usar-default-key)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```
Las claves por defecto comúnmente son solo agregadas a las sentencias nuevas funcionales
para que funcionen. Las claves en sí mismas son usualmente superfluas y desperdician recursos
sin ningún motivo. Pueden incluso llevar a errores difíciles de detectar porque ignoran
los tipos de datos numéricos.
Las sentencias `SORT` y `DELETE ADJACENT` sin una lista explícita de campos va a utilizar la 
clave primaria de la tabla interna, que en el caso de `DEFAULT KEY` puede provocar
resultados inesperados cuando tengas campos numéricos como componente de la clave,
en particular en combinación con `READ TABLE ... BINARY`, etc.

Especifica los componentes de la llave explícitamente

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```
o recurre a usar `EMPTY KEY` si no necesitas la clave para nada.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> Para más detalles, lee [Horst Keller's blog on _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)
> 
> **Precaución:** `SORT` en tablas internas con `EMPTY KEY` (sin campos de ordenamiento explícitos) no va a ordenar
> nada, pero aparecerán advertencias de síntaxis en caso de que se pueda determinar estáticamente que la clave está 
> vacía.

### Prefiere INSERT INTO TABLE a APPEND TO

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#prefiere-insert-into-table-a-append-to)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` funciona con todos los tipos de tabla y de clave,
por lo tanto haciendo más fácil refactorizar el tipo de tabla y definiciones clave si los requerimientos
de rendimiento cambian.

Utiliza `APPEND TO` únicamente si usas una tabla `STANDARD` en modo de arreglo,
si quieres dejar claro que el registro añadido será el último.

### Prefiere LINE_EXISTS a READ TABLE o LOOP AT

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#prefiere-line_exists-a-read-table-o-loop-at)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

expresa la intención más claro y corto que

```ABAP
" anti-pattern
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

o incluso

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Prefiere READ TABLE a LOOP AT

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#prefiere-read-table-a-loop-at)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

expresa la intención más claro y corto que

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

o incluso

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Prefiere LOOP AT WHERE a IF anidado

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#prefiere-loop-at-where-a-if-anidado)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

expresa la intención más claro y corto que

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Evita lecturas innecesarias a tablas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#evita-lecturas-innecesarias-a-tablas)

En caso de que _esperes_ que un registro esté ahí,
lee una vez y reacciona a la excepción,

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

en lugar de contaminar y alentar el flujo de control
principal con una lectura doble.

```ABAP
" anti-pattern
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = my_table[ key = input ].
```

> Además de ser una mejora de rendimiento,
> esta es una variante especial de la sección más general
> [Enfócate en el happy path o en manejo de errores, no en ambos](#enfócate-en-el-happy-path-o-en-manejo-de-errores-no-en-ambos).

## Strings

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#strings)

### Usa ` para definir literales

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Strings](#strings) > [Esta sección](#usa--para-definir-literales)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

Evita usar `'`, ya que hace una conversión de tipo superflua y confunde al lector
sobre si está lidiando con un `CHAR` o un `STRING`:

```ABAP
" anti-pattern
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` está generalmente bien, pero no puede ser usada para `CONSTANTS` y agrega coste innecesario cuando se especifica un valor fijo:

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### Usa | para construir textos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Strings](#strings) > [Esta sección](#usa--para-construir-textos)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

Las plantillas o templates de string resaltan mejor qué es un literal
y qué es una variable, especialmente si colocas múltiples variables en un texto.

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Booleanos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#booleanos)

### Usa los booleanos sabiamente

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Booleanos](#booleanos) > [Esta sección](#usa-los-booleanos-sabiamente)

Frecuentemente encontramos casos donde los booleanos naturalmente parecen ser la opción

```ABAP
" anti-pattern
is_archived = abap_true.
```

Hasta que un cambio de punto de vista sugiere que debimos haber elegido
una enumeración

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

Generalmente, los booleanos son una mala elección 
para distinguir tipos de cosas
porque casi siempre encontrarás casos que no
son exclusivamente uno u otro

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

[Separa los métodos, en lugar de recibir parámetros booleanos de entrada](#separa-los-métodos-en-lugar-de-recibir-parámetros-booleanos-de-entrada)
explica más a detalle por qué siempre debes cuestionarte usar
parámetros booleanos.

> Lee más en
> [1](http://www.beyondcode.org/articles/booleanVariables.html)

### Usa ABAP_BOOL para booleanos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Booleanos](#booleanos) > [Esta sección](#usa-abap_bool-para-booleanos)

```ABAP
DATA has_entries TYPE abap_bool.
```

No uses el tipo genérico `char1`. Aunque es técnicamente compatible,
esconde el hecho de que estamos lidiando con una variable booleana.

También evita otros tipos de booleanos ya que a menudo tienen efectos secundarios,
por ejemplo el tipo `boolean` usa un tercer valor llamado "undefined" que resulta
en sutiles errores de programación.

En algunos casos, puedes necesitar un elemento de diccionario de datos, por ejemplo
para campos de DynPro. `abap_bool` no puede ser usado en este caso porque está
definido en el type pool `abap`, no en el diccionario de datos.

En este caso, utiliza `boole_d` o `xfeld`.
Crea tu propio elemento de datos si necesitas una descripción personalizada.

> Puede que ABAP sea el único lenguaje de programación que no tiene un tipo de datos
> booleano universal.
> Sin embargo, tener uno es imperativo.
> Esta recomendación está basada en las ABAP Programming Guidelines.

### Usa ABAP_TRUE y ABAP_FALSE para hacer comparaciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Booleanos](#booleanos) > [Esta sección](#usa-abap_true-y-abap_false-para-hacer-comparaciones)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```
No uses los equivalentes en caracter `'X'` y `' '` o `space`;
hacen que sea más difícil identificar que es una expresión booleana:

```ABAP
" anti-pattern
has_entries = 'X'.
IF has_entries = space.
```

Evita comparaciones con `INITIAL` - fuerza al lector a recordar que el default de `abap_bool` es `abap_false`:

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> Puede que ABAP sea el único lenguaje de programación que no constantes integradas
> para verdadero y falso.
> Sin embargo, tenerlas es imperativo.
> Esta recomendación está basada en las ABAP Programming Guidelines.

### Usa XSDBOOL para asignar variables booleanas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Booleanos](#booleanos) > [Esta sección](#usa-xsdbool-para-asignar-variables-booleanas)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

El equivalente `IF`-`THEN`-`ELSE` es mucho más largo sin sentido:

```ABAP
" anti-pattern
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool` es la mejor manera para nuestro propósito, ya que directamente genera un `char1`,
lo que funciona perfecto con nuestro tipo `abap_bool`.
Las funciones equivalentes `boolc` y `boolx` producen tipos diferentes
y causan una conversión de tipo implícita innecesaria.

Estamos de acuerdo que el nombre `xsdbool` es desafortunado;
después de todo, no estamos interesados en el "XML Schema Definition" que sugiere el prefijo
"xsd".

Una posible alternativa a `xsdbool` es la forma ternaria de `COND`.
Su sintaxis es intuitiva, pero un poco más larga porque repite innecesariamente el
segmento `THEN abap_true`, y requiere conocimiento del valor implícito por 
default `abap_false` - razón por la cual lo sugerimos únicamente como
una solución secundaria.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## Condiciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#condiciones)

### Intenta hacer tus condiciones positivas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Condiciones](#condiciones) > [Esta sección](#intenta-hacer-tus-condiciones-positivas)

```ABAP
IF has_entries = abap_true.
```

Para fines de comparación, observa lo difícil que se vuelve entender la misma sentencia
invirtiéndola:

```ABAP
" anti-pattern
IF has_no_entries = abap_false.
```
El "intenta" en el título de la sección se refiere a que no debes forzar
este consejo al punto de que termines con algo como [ramas IF vacías](#no-dejes-ramas-if-vacías):

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> Lee más en _Capítulo 17: Síntomas y heurística: G29: Evitar condicionales negativas_ de [_Código Limpio_ por Robert C. Martin].

### Prefiere IS NOT sobre NOT IS

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Condiciones](#condiciones) > [Esta sección](#prefiere-is-not-sobre-not-is)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

La negación es lógicamente equivalente
pero requiere un "cambio mental de perspectiva"
que hace que sea más difícil de entender.

```ABAP
" anti-pattern
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> Una variante más específica de 
[Intenta hacer tus condiciones positivas](#intenta-hacer-tus-condiciones-positivas)
También descrito en la sección
[Alternative Language Constructs](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenalternative_langu_guidl.htm)
de las ABAP programming guidelines.

### Considera descomponer condiciones complejas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Condiciones](#condiciones) > [Esta sección](#considera-descomponer-condiciones-complejas)

Las condiciones pueden volverse más fáciles al descomponerlas en sus 
componentes elementales que las conforman:

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

en lugar de dejar todo en su lugar:

```ABAP
" anti-pattern
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> Usa los quick-fixes de ABAP Development Tools para rápidamente extraer condiciones y crear variables como las mostradas.

### Considera extraer condiciones complejas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Condiciones](#condiciones) > [Esta sección](#considera-extraer-condiciones-complejas)

Casi siempre es una buena idea extraer condiciones complejas a su propio método:

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

## Ifs

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#ifs)

### No dejes ramas IF vacías

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Ifs](#ifs) > [Esta sección](#no-dejes-ramas-if-vacías)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

es más corto y claro que

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### Prefiere CASE a ELSE IF para múltiples condiciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Ifs](#ifs) > [Esta sección](#prefiere-case-a-else-if-para-múltiples-condiciones)

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

`CASE` hace más fácil ver un conjunto de alternativas que se excluyen mutuamente.
Puede ser más rápido que una serie de `IF`s porque puede traducirse a una instrucción
de microprocesador diferente, en lugar de una serie de condiciones evaluadas subsecuentemente.
Puedes introducir casos nuevos rápidamente, sin tener que repetir la variable una y otra vez.
La sentencia incluso previene algunos errores que pueden ocurrir al anidar incorrectamente los `IF`-`ELSEIF`s.

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

### Mantén la profundidad del anidamiento baja

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Ifs](#ifs) > [Esta sección](#mantén-la-profundidad-del-anidamiento-baja)

```ABAP
" anti-pattern
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

Los `IF`s anidados se vuelven difíciles de entender muy rápidamente y requiren un número
de casos de prueba exponencialmente mayor para lograr cobertura completa del código.

Los árboles de decisión típicamente se pueden deshacer haciendo sub-métodos e introduciendo
variables auxiliares booleanas.

Otros casos pueden ser simplificados uniendo `IF`s, como


```ABAP
IF <this> AND <that>.
```

en lugar del innecesariamente anidado

```ABAP
" anti-pattern
IF <this>.
  IF <that>.
```

## Expresiones regulares

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#expresiones-regulares)

### Prefiere métodos simples a expresiones regulares

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Expresiones regulares](#expresiones-regulares) > [Esta sección](#prefiere-métodos-simples-a-expresiones-regulares)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

Las expresiones regulares se vuelven difíciles de entender muy rápidamente.
Los casos simples son usualmente más fáciles sin ellas.

Las expresiones regulares también usualmente consumen más memoria
y tiempo de procesamiento, porque necesitan ser analizadas gramaticalmente
en un árbol de expresiones y compiladas en tiempo de ejecución en un matcher ejecutable.
Las soluciones simples pueden estar mejor servidas con un `LOOP` y una variable temporal.

### Prefiere revisiones basis a expresiones regulares

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Expresiones regulares](#expresiones-regulares) > [Esta sección](#prefiere-revisiones-basis-a-expresiones-regulares)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```
en lugar de reinventar la rueda

```ABAP
" anti-pattern
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> Parece haber una tendencia natural al principio de Don't Repeat Yourself (DRY)
> o No Te Repitas cuando hay expresiones regulares en uso, 
> compara con la sección _Capítulo 17: Síntomas y heurística: General: G5: Duplicación_ en [_Código Limpio_ por Robert C. Martin].

### Considera construir expresiones regulares complejas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Expresiones regulares](#expresiones-regulares) > [Esta sección](#consider-assembling-complex-regular-expressions)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

Algunas expresiones regulares complejas se vuelven más sencillas
cuando demuestras al lector cómo están construidas a partir de sus piezas
elementales.

## Clases

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#clases)

### Classes: Object orientation

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Esta sección](#clases-orientación-a-objetos)

#### Prefiere objetos a clases estáticas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Clases: Orientación a objetos](#clases-orientación-a-objetos) > [Esta sección](#prefiere-objetos-a-clases-estáticas)

Las clases estáticas impiden todas las ventajas que se obtienen
utilizando programación orientada a objetos.
Especialmente vuelven cerca de imposible reemplazar dependencias productivas
con dobles de prueba en pruebas unitarias automatizadas.

Si piensas si debes hacer una clase o método estática, la respuesta casi siempre será:
no.

Una excepción típica a esta regla son las clases de utilerías.
Sus métodos hacen más fácil interactuar con ciertos tipos de ABAP.
No solo son completamente stateless, pero tan básicas que parecen
sentencias ABAP o funciones incluidas con ell lenguaje.
El factor discriminante es que sus consumidores las atan a su código
tan fuertemente que realmente no quieren hacerles un mock en sus pruebas unitarias.

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

#### Prefiere composición a herencia

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Clases: Orientación a objetos](#clases-orientación-a-objetos) > [Esta sección](#prefiere-composición-a-herencia)

Evita construir jerarquías de clases con herencia. En su lugar, prefiere la composición.

La herencia limpia es difícil de diseñar porque necesitas respetar reglas
como el [Principio de sustitución de Liskov](https://en.wikipedia.org/wiki/Liskov_substitution_principle).

También es difícil de entender porque las personas necesitan analizar y digerir los principios guía
detrás de la jerarquía. La herencia reduce el reuso, dado que los métodos tienden a solo estar 
disponibles para las sub-clases.
También complica la refactorización, porque mover o cambiar números tiende a requerir cambios a todo el 
árbol de la jerarquía.

La composición significa que diseñas objetos pequeños e independientes, donde cada uno sirve
un propósito específico. Estos objetos pueden ser recombinados en objetos más complejos mediante
simple delegación y patrones de facade.
La composición puede producir más clases, pero además de eso no tiene otras desventajas.

No permitas que esta regla te desanime de usar herencia cuando es correcto hacerlo.
Hay buenas applicaciones para herencia, por ejemplo el 
[Patrón de diseño Composite](https://en.wikipedia.org/wiki/Composite_pattern).
Solo pregúntate críticamente si la herencia en tu caso realmente proveerá más beneficios que desventajas.
Si tienes duda, la composición es generalmente la opción más segura.

> [Interfaces vs. clases abstractas](sub-sections/InterfacesVsAbstractClasses.md)
compara algunos detalles.

#### No mezcles lógica stateful y stateless en la misma clase

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Clases: Orientación a objetos](#clases-orientación-a-objetos) > [Esta sección](#no-mezcles-lógica-stateful-y-stateless-en-la-misma-clase)

No mezcles los paradigmas de programación stateless y stateful 
en la misma clase.

En programación stateless, los métodos reciben una entrada y producen una salida
_sin ningún efecto secundario_, resultando en métodos
que producen el mismo resultado sin importar
en qué orden fueron llamados.

```ABAP
CLASS /clean/xml_converter DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS convert
      IMPORTING
        file_contenido  TYPE xstring
      RETURNING
        VALUE(result) TYPE /clean/some_inbound_message.
ENDCLASS.

CLASS /clean/xml_converter IMPLEMENTATION.
  METHOD convert.
    cl_proxy_xml_transform=>xml_xstring_to_abap(
      EXPORTING
        xml       = file_contenido
        ext_xml   = abap_true
        svar_name = 'ROOT_NODE'
      IMPORTING
        abap_data = result ).
   ENDMETHOD.
ENDCLASS.
```

En programación stateful, manipulamos el estado interno del objeto
a través de sus métodos, lo que significa que está 
_llena de efectos secundarios_.

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

Ambos paradigmas están bien y cada uno tiene sus usos.
Sin embargo, _mezclarlos_ en el mismo objeto produce código
que es difícil de entender y seguro que fallará
con errores acarreados ocultos y problemas de 
sincronicidad.
No lo haga, compa.

### Alcance

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Esta sección](#alcance)

#### Global por default, solo local cuando sea apropiado

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Alcance](#alcance) > [Esta sección](#global-por-default-solo-local-cuando-sea-apropiado)

Usa clases globales por default.
Usa clases locales solo cuando sea apropiado.

> Las clases globales son las que son visibles en el diccionario de datos.
> Las clases locales viven dentro de un include de otro objeto de desarrollo y
> solo son visibles a ese objeto.

Las clases locales funcionan para

- estructuras de datos privadas muy específicas,
por ejemplo un iterador de los datos de la clase global,
que nunca será usado en otra clase más que esta,

- extraer un algoritmo privado complejo,
por ejemplo para desenredar esa estrategia de ordenamiento
y agregación que usa múltiples métodos del resto del
código de tu clase,

- para permitir hacer mock a ciertos aspectos de la clase global,
por ejemplo extrayendo todos los accesos de base de datos a una
clase local separada para que puedan ser reemplazados con
un doble de prueba en las pruebas unitarias.

Las clases locales evitan el reuso porque no pueden ser usadas
en ninguna otra parte.
Aunque son fáciles de extraer, la gente fallará incluso en encontrarlas, 
llevando a indeseable código duplicado.
Hacer debugging, navegar y orientarse en includes con clases locales
extensas es tedioso y molesto.

Ya que ABAP bloquea los objetos a nivel de include, la gente
no podrá trabajar en diferentes partes del include de manera simultánea
(lo cual sería posible si fueran clases globales separadas).

Reconsidera tu uso de clases locales si

- tu include local tiene decenas de clases y miles de líneas de código, 
- piensas de las clases globales como "paquetes" que almacenan otras clases,
- tus clases globales terminan siendo cascarones vacíos
- encuentras código duplicado a través de includes locales separados,
- tus desarrolladores comienzan a bloquearse unos a los otros y dejan de poder
trabajar en paralelo
- tus estimaciones de backlog están por los cielos porque tus equipos no logran entender
los sub-árboles locales de los otros miembros.

#### Marcar como FINAL si no fue diseñada para herencia

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Alcance](#alcance) > [Esta sección](#marcar-como-final-si-no-fue-diseñada-para-herencia)

Marca las clases que no están diseñadas explícitamente para herencia como `FINAL`.

Al diseñar cooperación entre clases, tu primera opción debería ser 
[composición, no herencia](#prefiere-composición-a-herencia).
Habilitar la herencia no es algo que se debe hacer a la ligera,
ya que requiere que pienses en temas como `PROTECTED` vs. `PRIVATE`
y el [Principio de sustitución de Liskov](https://en.wikipedia.org/wiki/Liskov_substitution_principle),
lo que congela muchos de los componentes internos.
Si no consideraste estas cosas en tu diseño de clase,
deberías evitar que alguien herede accidentalmente haciendo tu clase `FINAL`.

_Existen_ algunos ejemplos buenos de uso de herencia,
por ejemplo el patrón de diseño [composite](https://en.wikipedia.org/wiki/Composite_pattern).
Las BAdI también pueden volverse más útiles permitiendo las sub-clases,
habilitando al cliente para reutilizar la mayoría del código original.
Sin embargo, observa que todos estos casos tienen la herencia
incluida desde su diseño.

Clases no limpias que no [implementan interfaces](#los-métodos-de-instancia-públicos-deben-ser-parte-de-una-interfaz)
deberían de dejarse como no `FINAL` para permitir a los consumidores
hacer un mock de ellas en sus pruebas unitarias.

#### Miembros PRIVATE por default, PROTECTED solo si es necesario

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Alcance](#alcance) > [Esta sección](#miembros-private-por-default-protected-solo-si-es-necesario)

Haz atributos, métodos y otros miembros de tu clase `PRIVATE` por default.

Solo hazlos `PROTECTED` si quieres que las sub-clases puedan re-definirlos.

Los componentes internos de una clase solo se deben hacer disponibles 
a otros cuando sea estrictamente necesario.
Esto incluye no solamente consumidores externos, sino también las sub-clases.
Permitir que información de más esté disponible puede provocar errores
sútiles debido a re-definiciones inesperadas y complican la refactorización,
debido a que los consumidores externos congelan los miembros que aún 
deberían estar líquidos.

#### Considera usar inmutables en lugar de getters

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Alcance](#alcance) > [Esta sección](#considera-usar-inmutables-en-lugar-de-getters)

Un inmutable es un objeto que nunca cambia después de su construcción.
Para este tipo de objeto considera usar atributos públicos y `READ-ONLY`, en lugar
de métodos getter.

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

en lugar de

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

> **Precaución**: Para los objetos que **sí tienen** valores que cambian, no usar
atributos públicos `READ-ONLY`.
> De otra manera estos atributos se tienen que mantener actualizados,
> independientemente de si su valor se requiere por otro código o no.

#### Utiliza READ-ONLY con mesura

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Alcance](#alcance) > [Esta sección](#utiliza-read-only-con-mesura)

Varios lenguajes de programación modernos, especialmente Java, recomiendan hacer los miembros de la clase `READ-ONLY` donde sea apropiado para evitar efectos secundarios
accidentales.

Mientras que ABAP _sí_ odrece la adición `READ-ONLY` para declaraciones de datos,
recomendamos usarla con mesura.

La adición solamente está disponible en la `PUBLIC SECTION`, reduciendo su uso
drásticaamente. No puedes agregarla a la `PROTECTED SECTION` o `PRIVATE SECTION`,
ni a las variables locales en un método.

Además, funciona ligeramente diferente de lo que la gente podría esperar
de otros lenguajes de programación:
Los datos con la adición `READ-ONLY` pueden ser aún modificados desde cualquier 
método de la clase, sus `FRIENDS` y sus sub-classes.
Esto contradice el comportamiento más pervasivo de escribir-una-vez-nunca-modificar 
que podemos encontrar en otros lenguajes.
Esta diferencia puede llevar a malas sorpresas.

> Para evitar malentendidos: Proteger variables contra modificación accidental
es una buena práctica.
> Recomendaríamos aplicarlo a ABAP también si hubiera una sentencia
apropiada.

### Constructores

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Esta sección](#constructors)

#### Prefiere NEW a CREATE OBJECT

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Constructores](#constructores) > [Esta sección](#prefiere-new-a-create-object)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

a excepción de cuando requieras tipos dinámicos, por supuesto

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### Si tu clase global es CREATE PRIVATE, deja el constructor público

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Constructores](#constructores) > [Esta sección](#si-tu-clase-global-es-create-private-deja-el-constructor-público)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

Estamos de acuerdo que esto se contradice a sí mismo.

Sin embargo, de acuerdo al artículo
[_Instance Constructor_ of the ABAP Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm),
especificar el `CONSTRUCTOR` en la `PUBLIC SECTION` es requerido para
garantizar correcta compilación y validación de sintaxis.

Esto aplica solamente a clases globales.
En clases locales, haz el constructor privado, como debe ser.

#### Prefiere múltiples métodos de construcción estáticos a parámetros opcionales

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Constructores](#constructores) > [Esta sección](#prefiere-múltiples-métodos-de-construcción-estáticos-a-parámetros-opcionales)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP no soporta [sobrecarga](https://en.wikipedia.org/wiki/Function_overloading).
Usa variaciones de nombres y no parámetros opcionales para lograr
la semántica requerida.

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

La sección general
[_Separa métodos en lugar de agregar parámetros OPTIONAL_](#separa-métodos-en-lugar-de-agregar-parámetros-optional)
explica el razonamiento detrás de esto.

Considera resolver construcciones complejas en una creación
en varios pasos con el [Patrón de diseo Builder](https://en.wikipedia.org/wiki/Builder_pattern).

#### Usa nombres descriptivos para múltiples métodos de construcción

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Constructores](#constructores) > [Esta sección](#usa-nombres-descriptivos-para-múltiples-métodos-de-construcción)

Palabras adecuadas para comenzar métodos de creación son `new_`, `create_`, y `construct_`.
La gente intuitivamente lo conectan a la construcción de objetos.
También se prestan bien a frases con verbos como `new_from_template`, `create_as_copy`, o `create_by_name`.

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

en lugar de algo sin sentido como

```ABAP
" anti-pattern
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### Usa singletons únicamente cuando múltiples instancias no hacen sentido

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Constructores](#constructores) > [Esta sección](#usa-singletons-únicamente-cuando-múltiples-instancias-no-hacen-sentido)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

Aplica el patrón de diseño de [Singleton](https://en.wikipedia.org/wiki/Singleton_pattern) cuando tu diseño orientado a objetos
indica que tener una segunda instancia de algo no hace sentido.
Úsalo para asegurar que cada consumidor que está trabajando con la misma
clase tiene un objeto con el mismo estado y los mismos datos.

No uses el patrón Singleton por hábito o porque una regla de rendimiento
te indica que lo hagas.
Es el patrón de diseño más sobre-utilizado y mal aplicado y produce
efectos secundarios no esperados y complica innecesariamente las pruebas.
Si no hay razones de diseño para un objeto unitario, déjale esa decisión al
consumidor - él puede llegar a ese mismo resultado por fuera del constructor,
por ejemplo con un patrón de diseño de fábrica.

## Métodos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#métodos)

Estas reglas aplican a métodos en clases y módulos de funciones.

### Llamadas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#llamadas)

#### Prefiere llamadas funcionales a procedurales

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Calls](#calls) > [Esta sección](#prefiere-llamadas-funcionales-a-procedurales)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Si la programación dinámica prohibe llamadas funcionales, recurre al estilo procedural

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Varias de las reglas detalladas abajo son solo variaciones específicas de este
consejo.

#### Omite el uso de RECEIVING

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Calls](#calls) > [Esta sección](#omite-el-uso-de-receiving)

```ABAP
DATA(sum) = aggregate_values( values ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### Omite la palabra clave opcional EXPORTING

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Calls](#calls) > [Esta sección](#omite-la-palabra-clave-opcional-exporting)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Omite el nombre del parámetro en llamadas de un solo parámetro

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Calls](#calls) > [Esta sección](#omite-el-nombre-del-parámetro-en-llamadas-de-un-solo-parámetro)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates( list = list ).
```

Sin embargo, hay algunos casos donde el nombre del método no es suficiente
para que sea claro lo que se está haciendo, y el nombre del parámetro
puede dar la claridad necesaria:

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### Omite la referencia a sí mismo cuando llames un método de instancia

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Calls](#calls) > [Esta sección](#omite-la-referencia-a-sí-mismo-cuando-llames-un-método-de-instancia)

Ya que la referencia a la misma clase `me->` se coloca implícitamente por el sistema,
omite la sentencia cuando llames un método de instancia

```ABAP
DATA(sum) = aggregate_values( values ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
DATA(sum) = me->aggregate_values( values ).
```

### Methods: Object orientation

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#métodos-orientación-a-objetos)

#### Prefiere métodos de instancia a estáticos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Métodos: Orientación a objetos](#métodos-orientación-a-objetos) > [Esta sección](#prefiere-métodos-de-instancia-a-estáticos)

Los métodos deben ser miembros de instancia por default.
Los métodos de instancia reflejan mejor el estado de objeto de la clase.
Se puede crear un mock de ellos en pruebas unitarias.

```ABAP
METHODS publish.
```

Los métodos debe ser estáticos únicamente en casos excepcionales, como métodos
de creación estática.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### Los métodos de instancia públicos deben ser parte de una interfaz

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Métodos: Orientación a objetos](#métodos-orientación-a-objetos) > [Esta sección](#los-métodos-de-instancia-públicos-deben-ser-parte-de-una-interfaz)

Los métodos de instancia públicos deben _siempre_ ser parte de una interfaz.
Esto desacopla dependencias y simplifica crear un mock de ellos en pruebas unitarias.

```ABAP
METHOD /clean/blog_post~publish.
```
En la orientación a objetos limpia, tener un método público sin una interfaz no hace
sentido - con algunas excepciones como clases de enumeración, que nunca tendrán
una implementación alterna y nunca se les creará un mock.

> [Interfaces vs. clases abstractas](sub-sections/InterfacesVsAbstractClasses.md)
describe porque esto aplica también a las clases que sobre-escriben métodos heredados.

### Número de parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#número-de-parámetros)

#### Procura usar pocos parámetros IMPORTING, menos de tres es lo ideal

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Número de parámetros](#número-de-parámetros) > [Esta sección](#procura-usar-pocos-parámetros-importing-menos-de-tres-es-lo-ideal)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

sería mucho más claro que

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

Muchos parámetros de entrada permiten que la complejidad de un método explote
porque necesita manejar un número exponencial de combinaciones.
Muchos parámetros son un indicador de que el método puede estar haciendo
más de una sola cosa.

Puedes reducir el número de parámetros combinándolos en conjuntos que muestran
su significado a través de estructuras y objetos.

#### Separa métodos en lugar de agregar parámetros OPTIONAL

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Número de parámetros](#número-de-parámetros) > [Esta sección](#separa-métodos-en-lugar-de-agregar-parámetros-optional)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

para lograr la semántica deseada, ya que ABAP no soporta la [sobrecarga](https://en.wikipedia.org/wiki/Function_overloading).

```ABAP
" anti-pattern
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

Los parámetros opcionales confunden a los consumidores:

- ¿Cuáles se requieren de verdad?
- ¿Cuáles combinaciones son válidas?
- ¿Cuáles excluyen otros parámetros?

Se puede evitar esta confusión teniendo múltiples métodos con parámetros específicos,
dando una guía clara de que combinaciones de parámetros son validas y esperadas.

#### Usa PREFERRED PARAMETER con mesura

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Número de parámetros](#número-de-parámetros) > [Esta sección](#usa-preferred-parameter-con-mesura)

La adición `PREFERRED PARAMETER` hace difícil ver cuál parámetro se está enviando,
lo que hace más difícil entender el código.
Minimizar el número de parámetros, especialmente los opcionales,
automáticamente reduce la necesidad de `PREFERRED PARAMETER`.

#### Usa RETURNING, EXPORTING y CHANGING para exactamente un parámetro

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Número de parámetros](#número-de-parámetros) > [Esta sección](#usa-returning-exporting-y-changing-para-exactamente-un-parámetro)

Un buen método hace _una sola cosa_ y eso se debería ver reflejado en que
solo debe retornar exactamente una cosa.
Si los parámetros de salida de tu método _no_ forman una entidad lógica,
tu método está haciendo más de una cosa y debes dividirlo.

Hay casos donde la salida es una entidad lógica que consiste de múltiples
entidades. Este caso es fácilmente representado retornando una estructura
o un objeto:

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

en lugar de

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

Especialmente en comparación a múltiples parámetros `EXPORTING`, esto permite a la
gente usar el método funcional de llamado de métodos, evitando tener que pensar
en usar `IS SUPPLIED` y ayuda a las personas de accidentalmente olvidar
obtener información vital de `ERROR_OCCURRED`.

En lugar de múltiples parámetros opcionales de salida, considera dividir el método
de acuerdo a patrones de llamada que hagan sentido:

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

### Tipos de parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#tipos-de-parámetros)

#### Prefiere RETURNING en lugar de EXPORTING

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#prefiere-returning-en-lugar-de-exporting)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

En lugar del innecesariamente largo

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

`RETURNING` no solo hace la llamada más corta,
habilita el encadenamiento de métodos y previene [errores cuando la entrada y salida son iguales](#cuida-si-la-entrada-y-la-salida-pueden-ser-lo-mismo).

#### No hay problema generalmente con usar RETURNING para tablas grandes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#no-hay-problema-generalmente-con-usar-returning-para-tablas-grandes)

Aunque la documentación de ABAP y las guías de rendimiento dicen otra cosa,
rara vez hemos encontrado un caso donde una tabla grande o con estructura
profunda en un parámetro `VALUE` _realmente_ causen problemas de rendimiento.
Por lo tanto recomendamos usar generalmente

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

Únicamente si hay prueba (= una medición de mal rendimiento) para tu caso individual
es que deberías recurrir al estilo procedural

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

> Esta sección contradice las ABAP Programming Guidelines y las revisiones
> de Code Inspector, donde ambos sugieren que las tablas grandes deben estar
> en la sección `EXPORTING` para evitar problemas de rendimiento.
> Hemos fallado consistentemente en reproducir problemas de rendimiento y/o memoria
> y notado que hay optimizaciones de kernel que generalmente mejoran el rendimiento usando la sentencia `RETURNING`.

#### Usa solo RETURNING o EXPORTING, o CHANGING, pero no en conjunto

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#usa-solo-returning-o-exporting-o-changing-pero-no-en-conjunto)

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

en lugar de confundir con mezclas como

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

Diferentes tipos de parámetros de salida es un indicador de que el método hace más de una sola cosa.
Confunde al lector y hace llamar al método innecesariamente complicado.

Una excepción aceptable a esta regla son los builders que consumen su entrada mientras construyen su salida.

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

Sin embargo, incluso estos casos se pueden volver más claros convirtiendo la entrda
en un objeto:

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### Usa CHANGING con mesura, donde aplique

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#usa-changing-con-mesura-donde-aplique)

`CHANGING` debe estar reservado para casos donde una variable local existente que ya
está siendo llenada es actualizada solo en algunos lugares:

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

No forces a tus consumidores a introducir variables locales innecesarias solo para
llenar tu parámetro `CHANGING`.
No uses parámetros `CHANGING` para llenar una variable previamente vacía.

#### Separa los métodos, en lugar de recibir parámetros booleanos de entrada

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#separa-los-métodos-en-lugar-de-recibir-parámetros-booleanos-de-entrada)

Los parámetros de entrada booleanos son seguid un indicador de que un método
hace _dos_ cosas en lugar de una.

```ABAP
" anti-pattern
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

Además, las llamadas a métodos con un solo parámetro booleano tienden
a esconder el significado del parámetro

```ABAP
" anti-pattern
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

Dividir el método puede simplificar el código del método y describir
mejor las intenciones

```ABAP
update_without_saving( ).
update_and_save( ).
```

La percepción común sugiere que los setters para variables booleanas
son aceptables:

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> Lee más en
> [1](http://www.beyondcode.org/articles/booleanVariables.html)
> [2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/)
> [3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### Nombres de parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#nombres-de-parámetros)

#### Considera llamar RESULT al parámetro RETURNING

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Nombres de parámetros](#nombres-de-parámetros) > [Esta sección](#considera-llamar-result-al-parámetro-returning)

Los buenos nombres para métodos son usualmente _tan buenos_ que el parámetro 
`RETURNING` no necesita su propio nombre.
El nombre lograría poco menos que repetir el nombre del método o algo obvio.

Repetir el nombre de un miembro puede incluso producir conflictos que necesitan
ser resueltos agregando un superfluo `me->`

```ABAP
" anti-pattern
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

En estos casos, simplemente llama al parámetro `RESULT`, o algo como `R_RESULT` si prefieres la notación Húngara.

Nombra el parámetro `RETURNING` si _no_ es obvio su objetivo,
por ejemplo en métodos que retornan la instancia `me` para encadenamiento de métodos,
o en métodos que crean algo pero no retornan la entidad creada, sino solo su llave.

### Inicialización de parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#inicialización-de-parámetros)

#### Clear or overwrite EXPORTING reference parameters

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Inicialización de parámetros](#inicialización-de-parámetros) > [Esta sección](#limpia-o-sobre-escribe-parámetros-de-referencia-exporting)

Los parámetros por referencia apuntan a áreas de memoria existentes
que pueden estar llenas de antemano.
Limpia o sobre-escribelos para proveer información confiable.

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

> El Code Inspector and Checkman marcan variables `EXPORTING` que nunca se les asigna
un valor.
Usa estas revisiones estáticas para evitar este error difícil de resolver.

##### [Cuida si la entrada y la salida pueden ser lo mismo]

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Inicialización de parámetros](#inicialización-de-parámetros) > [Esta sección](#cuida-si-la-entrada-y-la-salida-pueden-ser-lo-mismo)

Generalmente, es una buena idea limpiar el parámetro como el primer paso después de
la declaración de datos y tipos.
Esto hace la sentencia fácil de ubicar y evita que el valor que aún está
contenido pueda ser usado accidentalmente en sentencias posteriores.

Sin embargo, algunas configuraciones de parámetro podrían usar la misma variable
como entrada y como salida.

En este caso, un `CLEAR` borraría el valor de entrada antes de que pueda ser usado,
produciendo resultados erróneos.

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

Considera rediseñar estos métodos reemplazando `EXPORTING` con `RETURNING`.
También considera sobre-escribir el parámetro `EXPORTING` en una sola sentencia
de cálculo.
Si ninguna se adecua al requerimiento, utiliza un `CLEAR` tardío.

#### No hagas CLEAR a parámetros VALUE

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Inicialización de parámetros](#inicialización-de-parámetros) > [Esta sección](#no-hagas-clear-a-parámetros-value)

Los parámetros que funcionan por `VALUE` son entregados como áreas de memoria
nuevas y separadas, que están vacías por definición.
No las limpies de nuevo:

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

Los parámetros `RETURNING` son siempre del tipo `VALUE`, así que nunca es necesario
hacerles un `CLEAR`:

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### Cuerpo del método

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#cuerpo-del-método)

#### Haz una cosa, hazla bien, no hagas más que eso

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Cuerpo del método](#cuerpo-del-método) > [Esta sección](#haz-una-cosa-hazla-bien-no-hagas-más-que-eso)

Un método debería hacer una cosa y una sola cosa.
Y lo debe hacer de la mejor manera posible.

Un método probablemente hace una cosa si:

- [tiene pocos parámetros](#procura-usar-pocos-parámetros-importing-menos-de-tres-es-lo-ideal)
- [no tiene parámetros booleanos de entrada](#separa-los-métodos-en-lugar-de-recibir-parámetros-booleanos-de-entrada)
- [tiene exactamente un parámetro de salida](#usa-returning-exporting-y-changing-para-exactamente-un-parámetro)
- [es corto](#mantén-los-métodos-cortos)
- [desciende un nivel de abstracción](#desciende-un-nivel-de-abstracción)
- [lanza un solo tipo de excepción](#lanza-un-solo-tipo-de-excepción)
- no puedes extraer más métodos de él con un significado claro
- no puedes agrupar sus sentencias en secciones lógicas

#### Enfócate en el happy path o en manejo de errores, no en ambos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Cuerpo del método](#cuerpo-del-método) > [Esta sección](#enfócate-en-el-happy-path-o-en-manejo-de-errores-no-en-ambos)

Como una especialización de la regla [_Haz una cosa, hazla bien, no hagas más que eso_](#haz-una-cosa-hazla-bien-no-hagas-más-que-eso), un método debería seguir
el happy path para el que fue construido o el camino de manejo de errores en
caso de que no pueda, pero probablemente no ambos.

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

Puede ser dividido en

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

o, para recalcar la parte de la validación

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

#### Desciende un nivel de abstracción

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Cuerpo del método](#cuerpo-del-método) > [Esta sección](#desciende-un-nivel-de-abstracción)

Las sentencias en un método deberían ser un nivel de abstracción más abajo
que el método mismo. De la misma manera, todas las sentencias deberían estar en el
mismo nivel de abstracción.

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```
en lugar de mezclas confusas de sentencias de bajo nivel (`trim`, `to_upper`, ...) y alto nivel (`publish`, ...) como

```ABAP
" anti-pattern
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

Una manera confiable de encontrar cuál es el nivel de abstracción adecuado
es la siguiente:
Permite al autor del método explicar lo que hace el método en pocas palabras,
sin que vea el código.
Los incisos que mencione son los submétodos que el método debería de llamar
o las sentencias que debería ejecutar.

#### Mantén los métodos cortos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Cuerpo del método](#cuerpo-del-método) > [Esta sección](#mantén-los-métodos-cortos)

Methods should have less than 20 statements, optimal around 3 to 5 statements.

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

La siguiente declaración `DATA` por si misma es suficiente para ver que el método
que la contiene hace más de una cosa:

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

Por supuesto hay ocasiones donde no hace sentido reducir un método grande más
de lo que ya está.
Esto es correcto siempre que el método [_haga una sola cosa_](#haz-una-cosa-hazla-bien-no-hagas-más-que-eso):

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

Sin embargo, aún hace sentido validar si el código extenso esconde
un patrón más adecuado:

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> Dividir los métodos hasta hacerlos muy pequeños puede tener un impacto negativo
en el rendimiento al incrementar el número de llamadas a métodos.
> La sección [_Considera el rendimiento_](#considera-el-rendimiento) provee guía en cómo balancear el Código Limpio y el rendimiento.

### Flujo de control

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#flujo-de-control)

#### Falla rápido

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Flujo de control](#flujo-de-control) > [Esta sección](#falla-rápido)

Valida y falla tan pronto como sea posible.

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

Las validaciones que se hacen después son más difíciles de ubicar y pueden
haber malgastado recursos para llegar a ese punto.

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

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Flujo de control](#flujo-de-control) > [Esta sección](#check-vs-return)

No hay un consenso acerca de si debes usar `CHECK` o `RETURN` para salir de un método
si las entradas no cumplen con los requisitos.

Mientras que `CHECK` definitivamente provee una sintaxis más corta

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

el nombre de la sentencia no revela lo que pasa si la condición falla,
de manera que las personas probablemente entenderán la forma larga mejor:

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD.
```

Puedes evitar la pregunta completamente invirtiendo la validación
y adoptando un flujo de control de un solo retorno

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD.
```

En cualquier caso, considera si no retornar algo es el comportamiento adecuado.
Los métodos deben proveer un resultado con significado, lo que implica un 
parámetro de retorno con un valor asignado o una excepción.
No retornar algo es en muchos casos similar a retornar `null`, lo que debería ser evitado.

> La sección [_Exiting Procedures_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)
> recomienda usar `CHECK` en este caso.
> La discusión en la comunidad sugiere que la sentencia no es tan clara,
> lo que causará que muchas personas no entiendan el comportamiento del programa.

#### Evita CHECK en otros lugares

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Flujo de control](#flujo-de-control) > [Esta sección](#evita-check-en-otros-lugares)

No uses `CHECK` fuera de la inicialización de un método.
La sentencia se comporta diferente en diferentes posiciones y puede llevar a efectos 
no esperados.

Por ejemplo,
[`CHECK` en un `LOOP` termina la iteración actual y continúa con la siguiente](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm);
las personas podrían accidentalmente esperar que se termine la ejecución del método
o que se salga del `LOOP`.
Es preferible usar una sentencia `IF` en combinación con un `CONTINUE`, ya que
`CONTINUE` solo puede usarse dentro de `LOOP`.

> Basado en la sección [_Exiting Procedures_ en las ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm).
> Observa que esto contradice [la referencia para la palabra clave `CHECK` en loops](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm).

## Manejo de errores

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#manejo-de-errores)

### Mensajes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#mensajes)

#### Haz que los mensajes sean fáciles de encontrar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Mensajes](#mensajes) > [Esta sección](#haz-que-los-mensajes-sean-fáciles-de-encontrar)

Para hacer los mensajes fáciles de encontrar en una búsqueda desde la transacción SE91, utiliza el siguiente patrón:

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

En caso de que la variable `message` no sea requerida, agrega el pragma `##NEEDED`:

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

Evita lo siguiente:

```ABAP
" anti-pattern
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

Este es un anti-patrón, ya que:
- Contiene código al que nunca se va a llegar.
- Prueba una condición que nunca puede ser `TRUE`.

### Códigos de retorno

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#códigos-de-retorno)

#### Prefiere excepciones a códigos de retorno

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Códigos de retorno](#códigos-de-retorno) > [Esta sección](#prefiere-excepciones-a-códigos-de-retorno)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

en lugar de

```ABAP
" anti-pattern
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

Las excepciones tienen múltiples ventajas sobre los códigos de retorno:

- Las excepciones mantienen el prototipo de tu método limpio:
puedes retornar el resultado de un método como un parámetro `RETURNING` y aún así
lanzar excepciones al mismo tiempo.
Los códigos de retorno contaminan tus prototipos con parámetros adicionales para 
manejo de errores.

- El consumidor no tiene que reaccionar a ellos inmediatamente.
Puede simplemente escribir el happy path de su código. La palabra para manejo
de excepciones `CATCH` puede estar hasta el final de su método o incluso afuera.

- Las excepciones pueden proveer detalles del error en sus atributos y a través 
de métodos. Los códigos de retorno requieren que encuentres una solución diferente
por tu cuenta, como también regresar un log.

- El ambiente le recuerda al consumidor con errores de sintaxis que tiene
que manejar excepciones. Los códigos de retorno pueden ser accidentalmente
ignorados sin que se dé cuenta alguien.

#### No dejes pasar los errores

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Return Codes](#códigos-de-retorno) > [Esta sección](#no-dejes-pasar-los-errores)

Si tienes que usar códigos de retorno, por ejemplo porque usas módulos de funciones y
código que no está bajo tu control, asegúrate de no dejar que los errores se escapen.

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
  RAISE EXCEPTION NEW /clean/some_error( ).
ENDIF.
```

### Excepciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#excepciones)

#### Las excepciones son para errores, no para casos regulares

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Excepciones](#excepciones) > [Esta sección](#las-excepciones-son-para-errores-no-para-casos-regulares)

```ABAP
" anti-pattern
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

Si algo es un caso regular y válido, debería ser manejado a través de 
parámetros de salida regulares.

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

Las excepciones deben estar reservadas para casos que no esperas y que reflejan
situaciones de error.

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

Usar mal las excepciones hace pensar al lector que algo falló, cuando en realidad
todo está bien.
Las excepciones son además mucho más lentas que el código regular porque necesitan
ser construidas y generalmente acumulan mucha información de contexto del sistema.

#### Usa excepciones basadas en clases

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Excepciones](#excepciones) > [Esta sección](#usa-excepciones-basadas-en-clases)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

Las excepciones clásicas no basadas en clases tienen las mismas características 
que los códigos de retorno y ya no deberían ser utilizadas.

```ABAP
" anti-pattern
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### Lanzamiento de excepciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#lanzamiento-de-excepciones)

#### Usa tus propias súper clases

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#usa-tus-propias-súper-clases)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```

Considera crear súper clases abstractas para cada tipo de excepción para tu
aplicación, en lugar de hacer una heredar de las clases principales de excepción.
Esto te permite manejar con un `CATCH` todas _tus excepciones_.
Te permite también agregar funcionalidad común a todas tus excepciones, como 
manejo especial de texto.
El `ABSTRACT` previene que las personas usen accidentalmente estos errores
no descriptivos directamente.

#### Lanza un solo tipo de excepción

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-un-solo-tipo-de-excepción)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

En la gran mayoría de los casos, lanzar múltiples tipos de excepción no tiene uso.
El consumidor usualmente no está interesado ni es capaz de distinguir las situaciones
de error. Por lo tanto, típicamente las va a manejar todas de la misma manera - 
si este es el caso, para que distinguirlas en primer lugar?

```ABAP
" anti-pattern
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

Una mejor solución para reconocer diferentes situaciones de error es usar un tipo de
excepción, pero agregar sub-clases que permitan (pero no requieran) reaccionar a
situaciones individuales de error, como está descrito en  [Usa sub-clases para permitir que el usuario de la clase distinga situaciones de error](#usa-sub-clases-para-permitir-que-el-usuario-de-la-clase-distinga-situaciones-de-error).

#### Usa sub-clases para permitir que el usuario de la clase distinga situaciones de error

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#usa-sub-clases-para-permitir-que-el-usuario-de-la-clase-distinga-situaciones-de-error)

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

Si hay varias situaciones de error, usa códigos de error en su lugar:

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

#### Lanza CX_STATIC_CHECK para excepciones que se pueden manejar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-cx_static_check-para-excepciones-que-se-pueden-manejar)

Si una excepción se espera que puede ocurrir y que puede razonablemente
ser manejada por el receptor, lanza una excepción con revisión, heredando
de `CX_STATIC_CHECK`: fallar validación de parámetros de entrada,
un recurso faltante para el cual hay una solución, etc.

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

Este tipo de escepción _debe_ aparecer en el prototipo del método y _debe_
ser atrapado o propagado para evitar errores de sintaxis.
Por lo tanto es claramente visible para el consumidor y asegura que no
será sorprendido por una excepción inesperada y podrá hacerse cargo
de la situación de error.

> Esto está en sincronización con las [ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm)
> pero contradice [_Código Limpio_ por Robert C. Martin],
> que recomienda preferir excepciones sin revisión;
> [Excepciones](sub-sections/Exceptions.md) explica por qué.

#### Lanza CX_NO_CHECK para situaciones de las que típicamente no se puede recuperar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-cx_no_check-para-situaciones-de-las-que-típicamente-no-se-puede-recuperar)

Si una excepción es tan severa que es poco probable que el receptor se recupere, 
usa `CX_NO_CHECK`: error al leer un recurso imprescindible, error al resolver 
la dependencia solicitada, etc.

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` _no puede_ ser declarada en el prototipo de los métodos,
de tal manera que al ocurrir será una mala sorpresa para el consumidor.
En el caso de situaciones de las cuales no se puede recuperar, esto está bien
porque el consumidor no tendrá la capacidad de hacer algo al respecto.

Sin embargo, _puede_ haber casos donde el consumidor quiere reconocer y reaccionar
a este tipo de error.
Por ejemplo, un administrador de dependencias podría lanzar un `CX_NO_CHECK` si
no le es posible proveer una implementación para una interfaz requerida
porque el código de la aplicación no podría continuar
Sin embargo, puede haber un reporte de prueba que trata de crear una instancia
de todos los tipos, solo para ver si funcionan; el cual reportará el fallo
simplemente como una entrada en rojo en una lista - este servicio debería
poder atrapar e ignorar la excepción en lugar de ser forzado a lanzar un dump.

#### Considera CX_DYNAMIC_CHECK para excepciones que no se pueden evitar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#considera-cx_dynamic_check-para-excepciones-que-no-se-pueden-evitar)

Los casos de uso para `CX_DYNAMIC_CHECK` son raros,
y en general recomendamos usar los otros tipos de excepción.
Sin embargo, puedes querer considerar este tipo de excepción
como un reemplazo para `CX_STATIC_CHECK` si el consumidor tiene control completo
y consciente de que una excepción puede ocurrir.

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

Por ejemplo, considera el método `get_db_length_decs`
de la clase`cl_abap_math`, que te dice el número de dígitos y decimales de un
número de punto flotante.
Este método lanza la excepción dinámica `cx_parameter_invalid_type`
si el parámetro de entrada no representa un número de punto flotante.
Usualmente, este método será llamado para una variable estática y completamente
tipificada, de tal manera que el desarrollador sabe si la excepción puede ocurrir 
o no. La excepción dinámica permitiría al consumidor
omitir la sentencia `CATCH` que no es necesaria en este caso.

#### Lanza un dump para situaciones que son completamente irrecuperables

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-un-dump-para-situaciones-que-son-completamente-irrecuperables)

Si una situación es tan severa que estás totalmente seguro que el consumidor
no se podrá recuperar de ella, o que claramente indica un error de programación, 
lanza un dump en lugar de una excepción: error al adquirir memoria, lectura fallida
de índices en una tabla que debe estar llena, etc.

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

This behavior will prevent any kind of consumer from doing anything useful afterwards.
Use this only if you are sure about that.

#### Prefiere RAISE EXCEPTION NEW en lugar de RAISE EXCEPTION TYPE

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#prefiere-raise-exception-new-en-lugar-de-raise-exception-type)

Nota: Disponible a partir de NW 7.52.

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

en general es más corto que el innecesariamente largo

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

Sin embargo, si haces mucho uso de la adición `MESSAGE`, podrías querer mantener el uso con la variante `TYPE`:

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception
  MESSAGE e136(messages).
```

### Atrapando excepciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#atrapando-excepciones)

#### Wrap foreign exceptions instead of letting them invade your code

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Atrapando excepciones](#atrapando-excepciones) > [Esta sección]Envuelve excepciones foráneas en lugar de permitir que invadan tu código

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

La [Ley de Demeter](https://en.wikipedia.org/wiki/Law_of_Demeter) recomienda
desacoplar las cosas.
Propagar excepciones de otros componentes viola este principio.
Vuélvete independiente de código externo atrapando estas excepciones
y propagándolas con tu propia excepción.

```ABAP
" anti-pattern
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## Comentarios

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#comentarios)

### Exprésate en código, no en comentarios

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#exprésate-en-código-no-en-comentarios)

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

en lugar de

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

Código Limpio _no_ te prohibe comentar tu código - te anima a explotar
_mejores_ maneras y recurrir a comentarios únicamente si éstas fallan.

> Este ejemplo ha sido señalado desde un punto de vista de rendimiento,
> mencionando que dividir tan pequeños los métodos lo empeora fuertemente.
> Mediciones de prueba demuestran que el código refactorizado es 2.13 veces más
> lento que la variante original. 
> La variante limpia toma 9.6 microsegundos para arreglar la entrada `31-02-2018`,
la variante no limpia toma 4.5 microsegundos.
> Este puede ser un problema cuando el método se corre muy seguido en una
> aplicación de alto rendimiento;
> para validación de parámetros de entrada regular, es aceptable.
> Recurre a la sección [Cuida el rendimiento](#cuida-el-rendimiento) para atender
> temas relacionados con Código Limpio y problemas de rendimiento.

### Los comentarios no son excusa para nombrar mal objetos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#los-comentarios-no-son-excusa-para-nombrar-mal-objetos)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

Improve your names instead of explaining what they really mean or why you chose bad ones.

```ABAP
" anti-pattern
" checks whether the table input contains entries
DATA(result) = check_table( input ).
```

### Usa métodos en lugar de comentarios para segmentar tu código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-métodos-en-lugar-de-comentarios-para-segmentar-tu-código)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

Esto no solo muestra la intención, estructura y dependencias mucho más claramente,
también evita acarrear errores cuando variables temporales no son limpiadas
adecuadamente entre secciones.

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

### Escribe comentarios para explicar el por qué, no el qué

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#escribe-comentarios-para-explicar-el-por-qué-no-el-qué)

```ABAP
" can't fail, existence of >= 1 row asserted above
DATA(first_line) = table[ 1 ].
```

_Nadie_ necesita repetir el código en lenguaje natural

```ABAP
" anti-pattern
" select alert root from database by key
SELECT * FROM d_alert_root WHERE key = key.
```

### El diseño va en los documentos de diseño, no en el código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#el-diseño-va-en-los-documentos-de-diseño-no-en-el-código)

```ABAP
" anti-pattern
" This class serves a double purpose. First, it does one thing. Then, it does another thing.
" It does so by executing a lot of code that is distributed over the local helper classes.
" To understand what's going on, let us at first ponder the nature of the universe as such.
" Have a look at this and that to get the details.
```

Nadie lee eso - en serio.
Si las personas necesitan leer una novela para usar tu código,
esto puede ser un indicador de que tu código tiene problemas severos de diseño
que deberías resolver.
Hay código que _sí necesita_ algo de explicación adicional más allá de una
sola línea de comentario; considera colocar la liga del documento de diseño 
en estos casos.

### Usa " para comentar, no *

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa--para-comentar-no-)

Los comentarios con comillas se indentan junto con las sentencias que están
siendo comentadas

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

Los comentarios con asteriscos tienden a indentarse de manera extraña

```ABAP
" anti-pattern
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### Usa comentarios antes de la sentencia a la que hacen referencia

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-comentarios-antes-de-la-sentencia-a-la-que-hacen-referencia)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

Más claro que

```ABAP
" anti-pattern
output = calculate_result( input ).
" delegate pattern
```

Y menos invasivo que

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### Borra el código en lugar de comentarlo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#borra-el-código-en-lugar-de-comentarlo)

```ABAP
" anti-pattern
* output = calculate_result( input ).
```

Cuando encuentres algo como esto, **bórralo**.
Este código obviamente no se necesita, porque tu aplicación funciona y todos las
pruebas están en verde.
Código borrado puede ser recuperado del historial de versiones si se necesita.
Si necesitas preservar una porción de código permanentemente, cópiala a un archivo
a `$TMP` o a `HOME`.

### Usa FIXME, TODO y XXX y agrega tu usuario

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-fixme-todo-y-xxx-y-agrega-tu-usuario)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME` indica errores que son muy pequeños o muy lejanos al propósito del 
incidente.
- `TODO` marca secciones donde quieres completar algo en el futuro(!) cercano.
- `XXX` indica código que funciona, pero podría mejorarse.

Cuando introduzcas un comentario de este estilo, agrega tu sobrenombre, 
iniciales o usuario para permitir que otros desarrolladores te contacten y
puedan hacer preguntas si el comentario no está claro.

### No agregues prototipos ni comentarios de fin de métodos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#no-agregues-prototipos-ni-comentarios-de-fin-de-métodos)

Los comentarios de prototipos de métodos no le sirven a nadie.

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

Hace décadas, cuando no podías ver el prototipo al inspeccionar el código o
que trabajabas con impresiones con docenas de páginas, estos comentarios puede
que hayan hecho sentido.
Pero todos los IDEs modernos (SE24, SE80, ADT) muestran la firma del método
fácilmente, de manera que estos comentarios no son nada más que ruido.

> En el editor basado en formas de la SE24/SE80, presiona el botón _Signature_.
> En ABAP Development Tools, selecciona el nombre del método y presiona F2
> o agrega la vista _ABAP Element Info_ a tu perspectiva en Eclipse.

Similarmente, los comentarios de fin de método son súperfluos.
Estos comentarios pueden haber sido útiles hace décadas,
cuando los programas, las funciones y los `IF`s anidados estaban dentro de cientos 
de líneas de código.
Pero nuestro estilo moderno de programación produce métodos lo suficientemente
cortos para que se pueda ver fácilmente la sentencia inicial a la que pertenece
un `ENDIF` o un `ENDMETHOD`:

```ABAP
" anti-pattern
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### No repitas los mensajes o textos en comentarios

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#no-repitas-los-mensajes-o-textos-en-comentarios)

```ABAP
" anti-pattern
" alert category not filled
MESSAGE e003 INTO dummy.
```

Los mensajes van a cambiar independientemente de tu código
y nadie va a recordar ajustar el comentario,
de tal manera que se va a desactualizar y va a confundir
a los desarrolladores rápidamente sin que nadie lo note.

Los IDEs modernos proveen maneras sencillas de ver el texto detrás de un mensaje,
por ejemplo en ABAP Development Tools,
seleccionas el ID de mensaje y presionas Shift + F2.

Si quieres que sea más explícito, considera extraer el mensaje a su
propio método.

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### Usa ABAP Doc únicamente para APIs públicas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-abap-doc-únicamente-para-apis-públicas)

Escribe ABAP Doc para documentar APIs públicas,
lo que significa APIs que están dirigidas a otros desarrolladores en otros
equipos o aplicaciones.
No escribas ABAP Doc para material interno.

ABAP Doc sufre de las mismas debilidades que todos los comentarios,
que se desactualizan rápidamente y solo confunden al desarrollador cuando pasa.
Como consecuencia, solo deberías usar esta herramienta cuando hace sentido, 
no obligar a que se use para todos los casos

> Lee más en _Capítulo 4: Comentarios: Javadoc en APIs públicas_ y en _Capítulo 4: Comentarios incorrectos:
> Javadocs in Nonpublic Code_ de [_Código Limpio_ por Robert C. Martin].

### Usa pragmas en lugar de pseudo-comentarios

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-pragmas-en-lugar-de-pseudo-comentarios)

Prefiere el uso de pragmas a pseudo-comentarios para suprimir advertencias
y errores irrelevantes identificados por ATC. Los pseudo-comentarios
están en su mayoría obsoletos y han sido reemplazados por pragmas.

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" anti-pattern
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

Usa el programa `ABAP_SLIN_PRAGMAS` o la tabla `SLIN_DESC` para encontrar el mapeo
entre pseudo-comentarios obsoletos y los pragmas que los han reemplazado.

## Formato

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#formato)

Las sugerencias en esta sección están [optimizadas para lectura, no para escritura](#optimiza-para-lectura-no-para-escritura).
Ya que el Pretty Printer de ABAP no las cubre, algunas de ellas provocan trabajo
adicional manual para re-formatear sentencias cuando las longitudes de los nombres
cambian, etc.; si quieres evitar esto, considera no usar reglas como 
As ABAP's Pretty Printer doesn't cover them, some of them produce additional manual work to reformat statements
when name lengths etc. change; if you want to avoid this, consider dropping rules like
[Alinea asignaciones al mismo objeto, pero no a objetos diferentes](#alinea-asignaciones-al-mismo-objeto-pero-no-a-objetos-diferentes).

### Sé consistente

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#sé-consistente)

Dale formato a todo el código de un proyecto de la misma manera.
Permite que todos los miembros del equipo usen el mismo estilo.

Si editas código externo, adhiérete al estilo de ese proyectom
en lugar de insistir en tu estilo personal.

Si las reglas de formato cambian con el tiempo,
usa [las mejores prácticas de refactorización](#cómo-refactorizar-código-legacy) 
para actualizar tu código.

### Optimiza para lectura, no para escritura

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#optimiza-para-lectura-no-para-escritura)

Los desarrolladores pasan la mayor parte de su tiempo _leyendo_ código.
En realidad, _escribir_ código toma parte de una porción muy pequeña del día.

Como consecuencia, deberías optimizar el formato de tu código para leer
y hacer debugging, no para escribir.

Por ejemplo, prefiere

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

a

```ABAP
" anti-pattern
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### Usa el Pretty Printer antes de activar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#use-the-pretty-printer-before-activating)

Aplica el Pretty Printer - Shift + F1 en SE80, SE24 y ADT - antes de activar
un objeto.

Si modificas código legacy sin formato,
puede que quieras aplicar el Pretty Printer solo a líneas selectas
para evitar generar listas de cambios enormes y dependencias
en transportes. Considera hacer Pretty Print en una orden 
de transporte individual o nota.

If you modify a larger unformatted legacy code base,
you may want to apply the Pretty Printer only to selected lines
to avoid huge change lists and transport dependencies.
Consider pretty-printing the complete development object
in a separate Transport Request or Note.

> Lee más en _Capítulo 5: Formato: Reglas de equipo_ de [_Código Limpio_ por Robert C. Martin].

### Usa la configuración de Pretty Printer de tu equipo 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-la-configuración-de-pretty-printer-de-tu-equipo)

Siempre usa la configuración de tu equipo.
Configúralo en
_Menu_ > _Utilities_ > _Settings ..._ > _ABAP Editor_ > _Pretty Printer_.

Configura _Indent_ y _Convert Uppercase/Lowercase_ > _Uppercase Keyword_
según se haya acordado en tu equipo.

> [Mayúsculas vs. Minúsculas](sub-sections/UpperVsLowerCase.md) explica 
> por qué no damos una guía clara sobre este tema.
>
> Lee más en _Capítulo 5: Formato: Reglas de equipo_ de [_Código Limpio_ por Robert C. Martin].

### No más de una sentencia por línea de código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#no-más-de-una-sentencia-por-línea-de-código)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

Aún si algunas situaciones pueden llevarte a creer que esto es legible:

```ABAP
" anti-pattern
DATA do_this TYPE i. do_this = input + 3.
```

### Mantén una longitud de línea razonable

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#mantén-una-longitud-de-línea-razonable)

Mantén una longitud máxima de línea de 120 caracteres.

El ojo humano lee el texto más cómodamente si las líneas no son tan anchas - 
pregunta a un diseñador de UI o investigador de movimiento del ojo de tu confianza.
También podrás apreciar mejor el código más angosto al hacer debugging o comparar
dos fuentes de código una junto a la otra.

El límite de 80 o incluso 72 caracteres originado en las terminales viejas
es muy restrictivo. Mientras que 100 caracteres son recomendados comúnmente y 
son una opción viable, para ABAP 120 funcionan un poco mejor, probablemente
debido a la verbosidad del lenguaje.

> Como recordatorio, puedes configurar ADT el margen a 120 caracteres
> que son visualizados en el código como una línea vertical.
> Configúralo en _Menu_ > _Window_ > _Preferences_ > _General_ > _Editors_ > _Text Editors_.

### Condensa tu código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#condensa-tu-código)

```ABAP
DATA(result) = calculate( items ).
```

en lugar de agregar espacios innecesarios

```ABAP
" anti-pattern
DATA(result)        =      calculate(    items =   items )   .
```

### Usa una línea en blanco para separar cosas, pero no más 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-una-línea-en-blanco-para-separar-cosas-pero-no-más)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

para resaltar que las dos sentencias hacen cosas diferentes. Pero no hay razón para

```ABAP
" anti-pattern
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```
La urgencia de agregar líneas en blanco puede ser un indicador de que tu método
no [hace una sola cosa](#haz-una-cosa-hazla-bien-no-hagas-más-que-eso).

### No te obsesiones con separar usando líneas en blanco 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#no-te-obsesiones-con-separar-usando-líneas-en-blanco)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

No hay razón para tener el mal hábito de separar tu código con líneaas en blanco

```ABAP
" anti-pattern
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

Las líneas en blanco solo hacen sentido si tienes una sentencia que usa varias líneas

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

### Alinea asignaciones al mismo objeto, pero no a objetos diferentes 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#alinea-asignaciones-al-mismo-objeto-pero-no-a-objetos-diferentes)

Para resaltar que estas cosas van juntas

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

o aún mejor

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

Pero no alinees cosas que no tienen nada que ver una con la otra:

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> Lee más en _Capítulo 5: Formato: Alineación horizontal_ de [_Código Limpio_ por Robert C. Martin].

### Cierra paréntesis en la última línea de código 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#cierra-paréntesis-en-la-última-línea-de-código)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### Mantén llamadas de un solo parámetro en una línea 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#mantén-llamadas-de-un-solo-parámetro-en-una-línea)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

en lugar del innecesariamente largo

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### Mantén los parámetros detrás de la llamada

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#mantén-los-parámetros-detrás-de-la-llamada)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Cuando esto haga las líneas muy largas, puedes hacer un salto de línea con los
parámetros a la siguiente línea:

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### Si haces un salto de línea, indenta parámetros debajo de la llamada 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#si-haces-un-salto-de-línea-indenta-parámetros-debajo-de-la-llamada)

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Alinear los parámetros en cualquier otra parte provoca que sea difícil
identificar a qué pertenecen:

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

Sin embargo, este es el mejor patrón si quieres evitar que el formato
se descomponga por el cambio de la longitud de un nombre.

### Usa saltos de línea para múltiples parámetros 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-saltos-de-línea-para-múltiples-parámetros)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Sí, esto desperdicia espacio.
Sin embargo, de otra manera, es difícil identificar dónde termina un parámetro y
dónde empieza el siguiente.

```ABAP
" anti-pattern
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### Alinea los parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#alinea-los-parámetros)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

No alinear los parámetros hace difícil ver donde termina el parámetro y comienza
su valor:

```ABAP
" anti-pattern
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> Por otro lado, este es el mejor patrón si quieres evitar que el formato se pierda
por un cambio de longitud en un nombre.

### Usa un salto de línea si la llamada a un método se vuelve muy extensa 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-un-salto-de-línea-si-la-llamada-a-un-método-se-vuelve-muy-extensa)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### Usa indentado apropiado

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-indentado-apropiado)

Indenta los parámetros de palabras clave con 2 espacios y los parámetros con 
4 espacios:

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

Si no tienes palabras clave, indenta con 4 espacios.
If you have no keywords, indent the parameters by 4 spaces.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Usa la tecla de Tab para indentar. Está bien si esto agrega un espacio más del
necesario. (Esto pasa si la parte de la izquierda en `DATA(sum) =` tiene un número
impar de caracteres).

### Indenta declaraciones in-line como llamadas a métodos 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#indenta-declaraciones-in-line-como-llamadas-a-métodos)

Indenta las declaraciones in-line con `VALUE` o `NEW` como si fueran llamadas a 
métodos:

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### No alinees los TYPE 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#no-alinees-los-type)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

Una variable y su tipo deben estar juntos y por lo tanto visualmente agrupados
uno próximo al otro.
Alinear los `TYPE` aleja la atención de eso e indica que las variables forman
un grupo vertical y sus tipos otro.
La alineación también provoca overhead innecesario de edición, requiriendo que
ajustes todas las indentaciones cuando el nombre de la variable más larga cambia.

```ABAP
" anti-pattern
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

## Testing

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#testing)

### Principios

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#principios)

#### Escribe código que se pueda probar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#escribe-código-que-se-pueda-probar)

Escribe todo tu código de manera que te permita probarlo automáticamente.

Si esto requiere que lo refactorices, hazlo.
Haz eso primero, antes de comenzar a agregar otras funcionalidades.

Si agregas funcionalidades a código legacy que está muy mal estructurado
para ser probado, refactorízalo por lo menos hasta que puedas probar
tu funcionalidad agregada.

#### Permite que otros hagan mock de tu código 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#permite-que-otros-hagan-mock-de-tu-código)

Si escribes tu propio código para ser consumido por otros, permite que escriban
pruebas unitarias para su propio código.
Esto se puede hacer, por ejemplo, agregando interfaces en todas las porciones 
de código públicas, proveyendo dobles de prueba que facilitan las pruebas de 
integración o aplicando inversión de dependencias para habilitarlos a que
sustituyan la configuración productiva por una de prueba.

#### Reglas de legibilidad  

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#reglas-de-legibilidad)

Haz tu prueba aún más legible que tu código productivo.
Puedes manejar código productivo malo con buenas pruebas, pero si no le entiendes
ni a las pruebas, estás perdido chavo.

Mantén tu código de prueba tan simple y estúpido que lo seguirás entendiendo 
dentro de un año.

Apégate a los estándares y patrones, para permitir que tus compañeros se
adapten rápidamente al código.

#### No hagas copias ni escribas reportes de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#no-hagas-copias-ni-escribas-reportes-de-prueba)

No comiences a trabajar en un requerimiento haciendo una copia en `$TMP` de un objeto 
de desarrollo.
Otros no notarán estos objetos y no sabrán el estatus de tu avance.
Probablemente desperdiciarás mucho tiempo haciendo que funcione la copia en primer
lugar.
Probablemente olvides borrar la copia después, dejando basura y dependencias en el
sistema.
(¿Crees que no pasa? Ve a tu sistema de desarrollo y revisa tu `$TMP` en este
momento)

No comiences escribiendo un reporte de prueba que llama algo de una manera
específica y que esa sea tu herramienta para verificar que las cosas siguen 
funcionando.
Estas son las pruebas del pobre: repitiendo un reporte de prueba a mano y verificando
visualmente que todo siga bien.

Toma el siguiente paso y automatiza este reporte en una prueba unitaria,
con una aserción automática que te diga si tu código aún está bien.
Te vas a salvar a ti mismo del esfuerzo de tener que escribir las pruebas unitarias
después.
Además, salvarás mucho tiempo de las repeticiones manuales y evitarás aburrirte
y cansarte de hacerlo.

#### Prueba componentes públicos, no los privados

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#prueba-componentes-públicos-no-los-privados)

Las partes públicas de las clases, especialmente las interfaces que implementan,
son relativamente estables y es poco probable que cambien.
Tus pruebas unitarias solo deberían validar los componentes públicos para que
sean robustas y minimicen el esfuerzo que tienes que hacer cuando refactorices
la clase.
Los componentes internos `PRIVATE` y `PROTECTED` pueden llegar a cambiar muy
rápido a través de refactorización, lo cual provocaría que tus pruebas
dejen de funcionar sin necesidad.

Una necesidad urgente de probar métodos `PRIVATE` o `PROTECTED` puede ser
una advertencia temprana de fallas en tu diseño.
Pregúntate:

- ¿Escondiste accidentalmente un concepto en tu clase que quiere salir en su 
propia clase, con su propio set de pruebas?

- ¿Olvidaste separar la lógica de dominio del código del glue code?
Por ejemplo, implementar la lógica de dominio directamente en la clase que
está conectada a BOPF como una acción, determinación o validación o que fue
generada por SAP Gateway como un proveedor de datos `*_DPC_EXT`, puede no ser
la mejor idea.

- ¿Tus interfaces son tan complicadas y requieren tantos datos irrelevantes que no
se le puede crear un mock fácilmente?

#### No te obsesiones con la cobertura del código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#no-te-obsesiones-con-la-cobertura-del-código)

La cobertura del código está ahí para ayudarte a encontrar código que olvidaste
probar, no para cumplir un KPI aleatorio.

No inventes pruebas con o sin aserciones dummy solo para llegar a una meta
de cobertura de código.

Es mejor dejar componentes sin pruebas para que sea transparente que no se pueden
refactorizar adecuadamente. Hay casos - como `IF`s en el constructor para insertar
dobles de prueba - que hacen impráctico llegar al 100%.
Las buenas pruebas tienden a cubrir la misma sentencia en múltiples ocasiones, para
diferentes bloques y condiciones.
De hecho, tendrán una cobertura imaginaria > 100%.

### Clases de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#clases-de-prueba)

#### Llama las clases locales de prueba de acuerdo a su objetivo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Clases de prueba](#clases-de-prueba) > [Esta sección](#llama-las-clases-locales-de-prueba-de-acuerdo-a-su-objetivo)

Nombra las clases de prueba por el "cuando" de la historia

```ABAP
CLASS ltc_<public method name> DEFINITION FOR TESTING ... ."
```

o por el "dado" de la historia

```ABAP
CLASS ltc_<common setup semantics> DEFINITION FOR TESTING ... .
```

```ABAP
" anti-patterns
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " We know that's the class under test - why repeat it?
CLASS ltc_test DEFINITION FOR TESTING ....                      " Of course it's a test, what else should it be?
```

#### Coloca tus pruebas en clases locales

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Clases de prueba](#clases-de-prueba) > [Esta sección](#coloca-tus-pruebas-en-clases-locales)

Coloca tus pruebas unitarias en el include de pruebas locales de la clase
bajo prueba. Esto asegura que las personas encuentren estas pruebas cuando
refactoricen la clase y permite que corran todos las pruebas asociadas
con un solo atajo de teclado, como está descrito en [Cómo ejecutar clases de prueba](#cómo-ejecutar-clases-de-prueba)


Coloca las pruebas de componente, integración y sistema en el include de prueba
local de una clase global separada. No se relacionan directamente a una sola
clase bajo prueba y por lo tanto no deberían ser colocadas arbitrariamente en
una de las clases involucradas, sino en una separada.

Marca esta clase global de prueba como `FOR TESTING` y `ABSTRACT` para evitar
que sea accidentalmente utilizada en código de producción. Poner pruebas
en otras clases tiene el peligro de que las personas no las vean
y olviden correr las pruebas antes de refactorizar.

Por lo tanto es beneficioso usar *test relations* para documentar cuáles objetos
esán siendo probados por la prueba.

Con el ejemplo de abajo de la clase de prueba `hiring_test`
se podría ejecutar al estar en la clase `recruiting` o `candidate` via el atajo
`Shift + Ctrl + F12` (Windows) o  `Cmd + Shift + F12` (macOS).

```abap
"! @testing recruiting
"! @testing candidate
class hiring_test definition
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### Coloca métodos de ayuda en clases de ayuda

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Clases de prueba](#clases-de-prueba) > [Esta sección](#coloca-métodos-de-ayuda-en-clases-de-ayuda)

Coloca métodos auxiliares usados por varias clases de prueba en una clase
auxiliar. Haz los métodos auxiliares disponibles mediante herencia o delegación.

```abap
" inheritance example

CLASS lth_unit_tests DEFINITION ABSTRACT.

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

#### Cómo ejecutar clases de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Clases de prueba](#clases-de-prueba) > [Esta sección](#cómo-ejecutar-clases-de-prueba)

En ABAP Development Tools, presiona `Ctrl + Shift + F10` para correr todas las
pruebas en una clase.
Presiona `Ctrl + Shift + F11` para incluir mediciones de cobertura.
Presiona `Ctrl + Shift + F12` para también correr pruebas en otras clases que
están mantenidas como relaciones.

> En macOS, usa `Cmd` en lugar de `Ctrl`.

### Código bajo prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#código-bajo-prueba)

#### Nombra el código bajo prueba con sentido, o usa CUT como default

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Código Bajo Prueba](#código-bajo-prueba) > [Esta sección](#nombra-el-código-bajo-prueba-con-sentido-o-usa-cut-como-default)

Nombra la variable que representa el código bajo prueba un nombre con un significado
adecuado:

```ABAP
DATA blog_post TYPE REF TO ...
```

No solo repitas el nombre de la clase con todos sus prefijos que no aportan valor:

```ABAP
" anti-pattern
DATA clean_fra_blog_post TYPE REF TO ...
```
Si tienes diferentes setups de prueba, puede ser de ayuda describir el estado variante
del objeto:

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

Si tienes problemas encontrando un nombre adecuado, usa `cut` como default. 
La abreviación proviene de _code under test_.

```ABAP
DATA cut TYPE REF TO ...
```

En pruebas confusas con código no limpio, llamar la variable `cut` puede
temporalmente ayudar al lector a identificar qué es lo que se está probando.
Sin embargo, limpiar las pruebas es la verdadera solución a largo plazo.

#### Prueba sobre interfaces, no implementaciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Código Bajo Prueba](#código-bajo-prueba) > [Esta sección](#prueba-sobre-interfaces-no-implementaciones)

Una consecuencia práctica de [_Prueba componentes públicos, no los privados_](#prueba-componentes-públicos-no-los-privados),
el tipo de tu código bajo prueba debe ser una _interfaz_.

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

en lugar de una _clase_

```ABAP
" anti-pattern
DATA code_under_test TYPE REF TO some_class.
```

#### Extrae la llamada al código bajo prueba a su propio método

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Código Bajo Prueba](#código-bajo-prueba) > [Esta sección](#extrae-la-llamada-al-código-bajo-prueba-a-su-propio-método)

Si el método a ser probado requiere muchos parámetros o preparación de datos,
puede ayudar extraer la llamada a un método auxiliar que ponga por default
los parámetros no relevantes para la prueba:

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

Llamar al método original directamente puede ensuciar tu prueba con muchos detalles
sin relevancia:

```ABAP
" anti-pattern
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### Inyección

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#inyección)

#### Usa inversión de dependencias para inyectar dobles de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#usa-inversión-de-dependencias-para-inyectar-dobles-de-prueba)

La inversión de dependencias significa que le entregas todas tus dependencias
al constructor_

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

No uses inyección por setter.
Permite que el código productivo sea usado en maneras que no se pretende:

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

No uses inyección mediante `FRIENDS`.
Esto inicializará las dependencias productivas antes de que sean reemplazadas,
probablemente provocando consecuencias no esperadas.
Don't use FRIENDS injection.
Además dejará de funcionar tan pronto renombres los componentes internos y
se salta las inicializaciones del constructor.

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

#### Considera usar la herramienta de ABAP test double

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#considera-usar-la-herramienta-de-abap-test-double)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

Más corto y sencillo que entender dobles de prueba personalizados:

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

#### Explota el uso de las herramientas de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#explota-el-uso-de-las-herramientas-de-prueba)

En general, un estilo de programación limpio te dejará hacer mucho del trabajo
con pruebas unitarias ABAP estándar y dobles de prueba.
Sin embargo, hay herramientas que te permitirán atacar casos más complejos:

- Usa el servicio `CL_OSQL_REPLACE` para probar sentencias complejas de
OpenSQL, redireccionándolas a una fuente de datos de prueba sin que las operaciones 
interfieran con el resto del sistema.

- Usa la librería de prueba de CDS para probar tus vistas CDS.

#### Usa test seams como una solución temporal

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#usa-test-seams-como-una-solución-temporal)

Si todas las otras técnicas fallan, o cuando estés en las aguas peligrosas del
código legacy, utiliza [test seams](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abaptest-seam.htm)
para que sea posible probar.

Aunque puede parecer cómodo de usar a primera vista, los test seams son invasivos
y tienden a enredarse con dependencias privadas, de tal manera que es difícil
mantenerlos vivos y estables en el largo plazo.

Por lo tanto recomendamos abstenerse de usar test seams, usándolos solo
como una solución temporal para permitir refactorizar el código en una forma
más apta para probarse.

#### Usa LOCAL FRIENDS para acceder al constructor inversor de dependencias

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#usa-local-friends-para-acceder-al-constructor-inversor-de-dependencias)

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

#### No uses LOCAL FRIENDS para invadir el código bajo prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-uses-local-friends-para-invadir-el-código-bajo-prueba)

Las pruebas unitarias que acceden componentes `PRIVATE` y `PROTECTED` para insertar
datos con un mock son frágiles: dejan de funcionar tan pronto la estructura interna
del código bajo prueba cambia.

```ABAP
" anti-pattern
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### No cambies el código productivo para poder probarlo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-cambies-el-código-productivo-para-poder-probarlo)

```ABAP
" anti-pattern
IF me->in_test_mode = abap_true.
```

#### No uses herencia para hacer mock a métodos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-uses-herencia-para-hacer-mock-a-métodos)

No uses herencia para sobre-escribir métodos y crearles un mock en tus pruebas.
Aunque esto funciona, es muy frágil ya que las pruebas dejan de funcionar al
refactorizar el código. Esto también permite que consumidores productivos de la
clase puedan heredar de ella [que puede tomarte desprevenido si no se planeó específicamente para ello](#marcar-como-final-si-no-fue-diseñada-para-herencia)

```ABAP
" anti-pattern
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

Para lograr usar código legacy para pruebas,
[utiliza test seams](#usa-test-seams-como-una-solución-temporal).
Son igual de frágiles, pero aún así una manera más limpia porque por lo menos no cambian el comportamiento productivo de la clase, como pasaría al habilitar
herencia quitando el `FINAL` o cambiando el alcance de un método de `PRIVATE` a `PROTECTED`.

Al escribir nuevo código, toma este punto de posibilidad de probar la clase 
directamente como parte de su diseño y encuentra una manera diferente y mejor.
Las mejores prácticas comunes incluyen [usar las herramientas de pruebas](#explota-el-uso-de-las-herramientas-de-prueba) y extraer el método problema
a una clase separada con su propia interfaz.

> Una variante más específica de
> [No cambies el código productivo para poder probarlo](#no-cambies-el-código-productivo-para-poder-probarlo).

#### No hagas mock sin necesidad

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-hagas-mock-sin-necesidad)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

Define tus "dados" tan precisamente como sea posible: no asignes información
que tu prueba no necesita y no le hagas mock a objetos que nunca se llaman.
Estas coss distraen al lector de lo que realmente está pasando.

```ABAP
" anti-pattern
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

También hay casos donde no es necesario crear un mock para nada - este es 
usualmente el caso con estructuras de datos y contenedores de datos.
Por ejemplo, tus casos de prueba pueden funcionar bien con la versión
productiva de `transient_log` porque solo almacena datos sin efectos secundarios.

#### No crees librerías para pruebas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-crees-librerías-para-pruebas)

Las pruebas unitarias - en contraste con las pruebas de integración - deberían de ser 
una caja negra con datos que entran y datos que salen, con todos los datos de 
prueba siendo definidos a medida que se requieren.

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

No comiences a construir librerías que distingan "*test case IDs*" para decidir
qué datos se van a usar. El código resultante será tan largo y enredado
que no te será posible mantener estas pruebas vivas en el largo plazo.

```ABAP
" anti-pattern

test_double->set_test_case( 1 ).

CASE me->test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### Métodos de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#métodos-de-prueba)

#### Nomenclatura para métodos de prueba: considera el supuesto y el resultado esperado

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#nomenclatura-para-métodos-de-prueba-considera-el-supuesto-y-el-resultado-esperado)

Nombres adecuados para métodos de prueba reflejan el "dado" y el "entonces"

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

Nombres poco útiles reflejan el "cuando", repiten hechos sin sentido o son crípticos:

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

Ya que ABAP solo permite 30 caracteres en nombres de métodos, es justo agregar
un comentario para explicar si el nombre es muy corto para transmitir el significado.
ABAP Doc o la primera línea en el método de prueba puede ser una elección apropiada
para el comentario.

Teniendo muchos métodos de prueba que tienen nombres muy largos puede ser un
indicador de que necesitas separar tu clase de prueba en varias y expresar
las diferencias en los "dados" de los nombres de las clases.

Having lots of test methods whose names are too long may be an indicator
that you should split your single test class into several ones
and express the differences in the givens in the class's names.

#### Usa dado-cuando-entonces

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#usa-dado-cuando-entonces)

Organiza tu código de prueba junto con el paradigma de dado-cuando-entonces:
Primero, inicializa las cosas en una sección donde se dan supuestos ("dado"),
después, llama el método a probar ("cuando"),
y finalmente valida el resultado ("entonces").

Si la sección de "dado" o "entonces" son tan extensas qeu no puedes separar
visualmente las tres secciones, refactoriza en sub-métodos.
Usar líneas blancas o comentarios como separadores puede parecer buena idea a primera
vista, pero no reduce la contaminación visual.
Aún así, son de ayuda para el lector y el escritor de pruebas novato para separar
las secciones.

#### "Cuando" es exactamente una llamada

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#cuando-es-exactamente-una-llamada)

Asegúrate que la sección de "cuando" de tu código de prueba contenga exactamente
una llamada a la clase bajo prueba:

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Llamar múltiples cosas indica que el método no tiene un enfoque claro y 
prueba demasiado.
Esto hace más difícil encontrar la causa cuando la prueba falla:
¿fue la primera, segunda o tercera llamada que causó el error?
También confunde al lector porque no está seguro de cuál es la funcionalidad
exacta que está siendo probada

#### No uses el método TEARDOWN a menos que realmente lo necesites

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#no-uses-el-método-teardown-a-menos-que-realmente-lo-necesites)

Los métodos `teardown` son usualmente necesarios para limpiar entradas
en base de datos u otros recursos en pruebas de integración.

Reiniciar miembros de la clase de prueba, especialmente `cut` y los dobles de prueba, es superfluo; son sobre-escritos por el método `setup` antes de que se ejecute
el siguiente método de prueba.

### Datos de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#datos-de-prueba)

#### Haz que sea fácil detectar la intención

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Datos de prueba](#datos-de-prueba) > [Esta sección](#haz-que-sea-fácil-detectar-la-intención)

En pruebas unitarias, quieres poder identificar rápidamente qué datos y dobles
son importantes y cuales solo están ahí para que el código no falle.
Permite esto dándole valores que no tienen un significado específico a los datos
de prueba, por ejemplo:

```ABAP
DATA(alert_id) = '42'.                             " well-known meaningless numbers
DATA(detection_object_type) = '?=/"&'.             " 'keyboard accidents'
CONSTANTS some_random_number TYPE i VALUE 782346.  " revealing variable names
```

No engañes al lector haciéndole creer que algo se conecta a objetos reales
o customizing real, si no lo hace:

```ABAP
" anti-pattern
DATA(alert_id) = '00000001223678871'.        " this alert really exists
DATA(detection_object_type) = 'FRA_SCLAIM'.  " this detection object type, too
CONSTANTS memory_limit TYPE i VALUE 4096.    " this number looks carefully chosen
```

#### Haz que sea fácil detectar las diferencias

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Datos de prueba](#datos-de-prueba) > [Esta sección](#haz-que-sea-fácil-detectar-las-diferencias)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

No forces al lector a comparar strings extensos para detectar diferencias mínimas.

#### Usa constantes para describir el objetivo y la importancia de los datos de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Datos de prueba](#datos-de-prueba) > [Esta sección](#usa-constantes-para-describir-el-objetivo-y-la-importancia-de-los-datos-de-prueba)

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

### Aserciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#aserciones)

#### Usa pocas aserciones, enfocadas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#usa-pocas-aserciones-enfocadas)

Haz una aserción únicamente sobre lo que se trata el método de prueba y con un número
pequeño de aserciones.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```
Hacer muchas aserciones es un indicador de que el método no tiene un enfoque claro.
Esto acopla código productivo y de prueba en muchos lugares: cambiar una funcionalidad
requerirá reescribir un número largo de pruebas, aunque realmente no tengan que ver
con la funcionalidad.
También confunde al lector con una variedad larga de aserciones, escondiendo
la aserción importante.

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

#### Usa el tipo de aserción correcto

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#usa-el-tipo-de-aserción-correcto)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

Las aserciones usualmente hacen más de lo que parece, por ejemplo `assert_equals`
incluye comparación de tipos y provee descripciones precisas si los valores
son diferentes.
Usar el tipo incorrecto te forzará inmediatamente a usar el debugger
en lugar de permitirte ver que está mal desde el mensaje de error.

```ABAP
" anti-pattern
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### Haz aserciones sobre el contenido, no la cantidad

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#haz-aserciones-sobre-el-contenido-no-la-cantidad)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

No escribas aserciones con números mágicos si puedes expresar el contenido actual
que esperas.
Los números pueden variar, aunque las expectativas se cumplan.
A la inversa, los números pueden cuadrar aunque el contenido sea completamente
inesperado.

```ABAP
" anti-pattern
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### Haz aserciones sobre la calidad, no el contenido

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#haz-aserciones-sobre-la-calidad-no-el-contenido)

Si estás interesado en una meta característica del resultado, pero no en el contenido
real, expresa esto con una aserción adecuada:

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

Hacerle una aserción al contenido esconde lo que realmente quieres probar. También
es frágil, porque refactorizar pueede producir un resultado diferente
pero perfectamente aceptable, aunque provoque que tus pruebas unitarias precisas
dejen de funcionar.

```ABAP
" anti-pattern
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### Usa FAIL para evaluar excepciones esperadas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#usa-fail-para-evaluar-excepciones-esperadas)

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

#### Propaga excepciones inesperadas en lugar de atraparlas y enviar error

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#propaga-excepciones-inesperadas-en-lugar-de-atraparlas-y-enviar-error)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

Tu código de prueba se mantiene enfocado en el happy path y por lo tanto es mucho 
más fácil de leer y entender, comparado con:

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

#### Escribe aserciones a la medida para reducir el código y evitar duplicados

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#escribe-aserciones-a-la-medida-para-reducir-el-código-y-evitar-duplicados)

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

En lugar de copiar y pegar una y otra vez.
