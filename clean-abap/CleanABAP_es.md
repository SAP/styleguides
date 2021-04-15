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
    - [No dejes pasar las fallas](#no-dejes-pasar-las-fallas)
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

Te recomendamos comenzar con cosas que sean fáciles de atender y ampliamente aceptadas,
como [Booleanos](#booleanos), [Condiciones](#condiciones), and [Ifs](#ifs).

Probablemente te beneficiarás más de la sección [Métodos](#métodos),
especialmente [Haz una cosa, hazla bien, solo haz eso](#haz-una-cosa-hazla-bien-solo-haz-eso) and [Mantén los métodos cortos](#mantén-los-métodos-cortos), ya que mejoran tremendamente la estructura de tu código.

Algunos temas de esta guía pueden iniciar conversaciones difíciles en los equipos
que tienen experiencia en lo que hacen, pero son nuevos al código limpio.
Estas temas son perfectamente "sanos", pero la gente puede tener problemas
adaptándose a ellos en el principio.

Una vez que domines los primeros temas, pasa a otros más controversiales;
ya que especialmente [Comentarios](#comentarios), [Nomenclatura](#nomenclatura), y [Formato](#formato)
pueden llevar a discusiones casi religiosas
y solo deberían ser utilizadas por equipos que ya tengan prueba de los efectos positivos del código limpio.

### Cómo refactorizar código legacy

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción) > [Esta sección](#cómo-refactorizar-código-legacy)

Los temass [Booleanos](#booleanos), [Condiciones](#condiciones), [Ifs](#ifs),
y [Métodos](#métodos) son los que más te recompensan al trabajar con un proyecto legacy con mucho código
que no puedes o no quieres cambiar, dado que puede ser aplicado solo al código nuevo sin conflicto.

El tema [Nomenclatura](#nomenclatura) es muy demandante para proyectos legacy,
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
No te obsesiones dedicando horas en "limpiar el campamento", solo
dedica un par de minutos adicionales y observa como las mejoras se acumulan en el tiempo.

3. Construye _islas limpias_: de vez en cuando, elige un pequeño objeto o componente y trata
de hacerlo limpio en todos los sentidos. Estas islas demuestran el beneficio de lo que estás
haciendo y forman una base solidamente probada para hacer más refactorizaciones en el futuro.

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
con la cual esta guía es compatible en su mayor parte. Las desviaciones a esa guía siempre están indicadas
y son siempre con el objetivo de tener código más limpio.

Esta guía también respeta las 
[Recomendaciones de DSAG para desarrollo ABAP](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf),
aunque es más precisa en la mayoría de los casos.

Desde su publicación, Clean ABAP se ha convertido en una guía de referencia para muchos
de los equipos de desarrollo de SAP in-house, incluyendo los cientos de desarrolladores
que trabajan en S/4HANA.

### Cómo estar en desacuerdo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Introducción](#introducción) > [Esta sección](#cómo-estar-en-desacuerdo)

Escribimos esta guía de estilo para lectores que ya están familiarizados con Clean Code o que están en el proceso de hacerlo, con un enfoque específico en cómo aplicar código limpio _específicamente para ABAP_.

Por este motivo, por favor toma en cuenta que no introducimos todos los conceptos en el mismo detalle
que el libro original y las fuentes mencionadas: estas fuentes aún vale la pena que sean leídas, especialmente
si no estás de acuerdo con elementos de esta guía porque no los explicamos tan bien.
Usa las ligas en las secciones para leer el trasfondo de nuestra guía.

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

> Lee más en el _Capítulo 2: Nombres con sentido: Usar nombres que revelen las intencioness_ de [_Código Limpio_ por Robert C. Martin].

### Prefiere nombres del dominio de solución o de problema

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#prefiere-nombres-del-dominio-de-solución-o-de-problema)

Busca nombres buenos en el dominio de solución, por ejemplo en ciencias de la computación términos como
"pila" o "árbol"; y en el dominio de problema, por ejemplo "cuenta" o "libro_mayor".

Las capas que pertenecen a negocio cuando son nombradas de acuerdo al dominio de problema.
Esto especialmente cierto para componetes que son diseñados bajo el diseño guiado por el dominio, como las APIs
y los objetos de negocio.

Las capas que proveen la mayor parte de la funcionalidad técnica, como las clases de fábrica y algoritmos abstractos, 
sonarán mejor cuando son nombradas de acuerdo al dominio de solución.

En cualquier caso, no trates de hacer tu propio lenguaje. Necesitamos poder intercambiar información
entre desarrolladores, product owners y clientes, así que elige nombres que todos entiendan
sin requerir un diccionario a la medida.

> Lee más en _Capítulo 2: Nombres con sentido: Usar nombres de dominios de soluciones_ y _[...]:
> Usar nombres de dominios de problemas_ de [_Código Limpio_ por Robert C. Martin].

### Usa plural

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-plural)

Hay una práctica legacy en SAP en la que nombran las tablas de las entidades en singular,
por ejemplo `country` para una "tabla de países".
Una tendencia común en el mundo fuera de SAP es usar plural para listas de cosas.
Por lo tanto, recomendamos elegir `countries` en su lugar.

> Este consejo es principalmente para cosas como variables y propiedades. 
> Para objetos de desarrollo, puede haber patrones
> que también hacen sentido, por ejemplo la convención ampliamente usada
> de nombrar tablas de bases de datos ("tablas transparentes") en singular.

> Lee más en _Capítulo 2: Nombres con sentido: Usar nombres que revelen las intenciones_ de [_Código Limpio_ por Robert C. Martin].

### Usa nombres pronunciables

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Nomenclatura](#nomenclatura) > [Esta sección](#usa-nombres-pronunciables)

Pensamos y hablamos mucho de objetos, así que usa nombres que puedas pronunciar,
por ejemplo, prefiere `tipos_de_objeto_de_detección` a algo ininteligible como `tipobjdet`

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

Las personas buscan usando palabras claves para encontrar código relevante.
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
verdad los estés usando. Por ejemplo, no le llames a tu clase `file_factory` a menos que realmente
implemente el patrón de diseño de fábrica.
Los patrones más comunes son:
[singleton](https://en.wikipedia.org/wiki/Singleton_pattern),
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern),
[facade](https://en.wikipedia.org/wiki/Facade_pattern),
[composite](https://en.wikipedia.org/wiki/Composite_pattern),
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern),
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern),
[observer](https://en.wikipedia.org/wiki/Observer_pattern), and
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
VAlida que la guía que quieres seguir en la versión más vieja que tienes que soportar.
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

Sin embargo, recomendamos fuertemente no optimizar prematuramente, basado en miedos irracionales.
La gran mayoría de las reglas (ej. nomenclatura, comentarios) no tiene impacto negativo.
Trata de construir las cosas de una manera limpia, orientada a objetos.
Si algo es muy lento, haz una medición de rendimiento.
Solo entonces deberías tomar una decisión, basada en hechos, para descartar reglas.

Algunos pensamientos adicionales, tomados en parte del Capítulo 2 de [_Refactoring_ por Martin Fowler](https://martinfowler.com/books/refactoring.html):

En una aplicación típica la mayoría del tiempo de ejecución se dedica en una proporción muy
pequeña del código. Tan poco como 10% del código puede consumir el 90% del tiempo de ejecución
y especialmente en ABAP una proporción grande del tiempo de ejecución se suele
dedicar en base de datos.

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
y pueden ser refactorizados y probados más fácil que el código procedural (funciones, programas).
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
asegúrate de revisar elementos obsoletos del lenguate
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
en código más limpio

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

Si coleccionas constantes de una manera desordenada, por ejemplo en una interfaz, agrúpalas:

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
ya que cada línea se ve diferente y cambiarlas require lidiar con 
comas, puntos, dos puntos, lo cual no vale la pena.

Chaining also needlessly complicates reformatting and refactoring
because each line looks different and changing them requires meddling with
colons, dots, and commas, that are not worth the effort.

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO reader.
```

> También lee [Don't align type clauses](#dont-align-type-clauses)  
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

Las revisiones de código demuestran que la gente tiene a escoger entre las dos arbitrariamente,
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
De la misma manera, la velocidad no es un problema. Como consecuencia, no hay razón relacionada con rendimiento
para preferir uno sobre el otro.

> Lee más en el artículo
> [_Accessing Data Objects Dynamically_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm).

## Tablas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#tablas)

### Usa el tipo correcto de tabla

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#usa-el-tipo-correcto-de-tabla)

- Típicamente se usan tablas del tipo `HASHED` para **tablas grandes** que son 
**llenadas en un solo paso**, **nunca se modifican** y son **leídas seguido por su llave**.
Su overhead en memoria y procesamiento hace a las tablas hash únicamente valiosas
cuando se tienen cantidades grandes de datos y muchos accesos de lectura.
Cada cambio al contenido d ela tabla requiere costosos recálculos del hash,
así que no uses este tipo para tablas que son modificadas muy seguido.

- Típicamente se usan tablas del tipo `SORTED` para **tablas grandes**
que necesitan estar **ordenadas en todo momento**, que son **llenadas poco a poco** o que 
**necesitan ser modificadas** y **leídas por una o más llaves parciales o completas** o procesadas
**en cierto orden**.
Agregar, cambiar o quitar contenido require encontrar el punto de inserción adecuado,
pero no requiere reajustar los índices de la tabla completa.
Las tablas ordenadas demustran su valor únicamente para cantidades grandes de accesos de lectura.

- Usa tablas del tipo `STANDARD` para **tablas pequeñas**, donde el indexado genera más overhead
que beneficio, y **arreglos**, donde no te interesa el orden de las filas o quieres procesarlas
exactamente en el orden que se agregaron a la tabla. Además, si se requieren diferentes tipos
de acceso a la tabla (por ejemplo, acceso por índice y acceso ordenado via `SORT` y `BINARY SEARCH`)

> Estas es una guía a grandes rasgos.
> Encuentra más detalles en [_Selection of Table Category_ in the ABAP Language Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm).

### Evita usar DEFAULT KEY

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#evita-usar-default-key)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```
Las llaves default comúnmente son solo agregadas a las sentencias nuevas funcionales
para que funcionen. Las llaves en sí mismas son usualmente superfluas y desperdician recursos
sin ningún motivo. Pueden incluso llevar a errores difíciles de detectar porque ignoran
los tipos de datos numéricos.
Las sentencias `SORT` y `DELETE ADJACENT` sin una lista explícita de campos va a utilizar la 
llave primaria de la tabla interna, que en el caso de `DEFAULT KEY` puede provocar
resultados inesperados cuando tengas campos numéricos como componente de la llave,
en particular en combinación con `READ TABLE ... BINARY`, etc.

Especifica los componentes de la llave explícitamente

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```
o recurre a usar `EMPTY KEY` si no necesitas la llave para nada.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> Para más detalles, lee [Horst Keller's blog on _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)
> 
> **Precaución:** `SORT` en tablas internas con `EMPTY KEY` (sin campos de ordenamiento explícitos) no va a ordenar
> nada, pero aparecerán advertencias de síntaxis en caso de que se pueda determinar estáticamente que la llave está 
> vacía.

### Prefiere INSERT INTO TABLE a APPEND TO

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Tablas](#tablas) > [Esta sección](#prefiere-insert-into-table-a-append-to)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` funciona con todos los tipos de tabla y de llave,
por lo tanto haciendo más fácil refactorizar el tipo de tabla y definiciones llave si los requerimientos
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

`|` está generalmente bien, pero no puede ser usada para `CONSTANTS` y agrega overhead innecesario cuando se especifica un valor fijo:

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### Usa | para construir textos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Strings](#strings) > [Esta sección](#usa--para-construir-textos)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

Las plantillas o templates de string resaltan mejor qué es una literal
y qué es una variable, especialmente si colocas múltiples variables en un texto.

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Booleanos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#booleanos)

### Usa los booleanos sabiamente

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Booleanos](#booleanos) > [Esta sección](#usa-los-booleanos-sabiamente)

Seguido encontramos casos donde los booleanos naturalmente parecen ser la opción

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

También evita otros tipos de booleanos ya que seguido tienen efectos secundarios,
por ejemplo el tipo `boolean` usa un tercer valor llamado "undefined" que resulta
en sútiles errores de programación.

En algunos casos, puedes necesitar un elemento de diccionario de datos, por ejemplo
para campos de DynPro. `abap_bool` no puede ser usado en este caso porque está
definido en el type pool `abap`, no en el diccionario de datos.

En este caso, utiliza `boole_d` o `xfeld`.
Crea tu propio elemento de datos si necesitas una descripción personalizada.

> Puede que ABAP sea el único lenguaje de programación que no tiene un tipo de dato
> universal booleano.
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

Evita comparaciones con `INITIAL` - forza al lector a recordar que el default de `abap_bool` es `abap_false`:

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> Puede que ABAP sea el único lenguaje de programación que no tiene un tipo de dato
> universal booleano.
> Sin embargo, tener uno es imperativo.
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

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Clases](#clases) > [Clases: Orientación a objetos](#clases-orientación-a-objetos) > [Esta sección] > (#no-mezcles-lógica-stateful-y-stateless-en-la-misma-clase)

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

instead of confusing mixtures like

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

Different sorts of output parameters is an indicator that the method does more than one thing.
It confuses the reader and makes calling the method needlessly complicated.

An acceptable exception to this rule may be builders that consume their input while building their output:

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

However, even those can be made clearer by objectifying the input:

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### Usa CHANGING con mesura, donde aplique

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#usa-changing-con-mesura-donde-aplique)

`CHANGING` should be reserved for cases where an existing local variable
that is already filled is updated in only some places:

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

Do not force your callers to introduce unnecessary local variables only to supply your `CHANGING` parameter.
Do not use `CHANGING` parameters to initially fill a previously empty variable.

#### Separa los métodos, en lugar de recibir parámetros booleanos de entrada

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Tipos de parámetros](#tipos-de-parámetros) > [Esta sección](#separa-los-métodos-en-lugar-de-recibir-parámetros-booleanos-de-entrada)

Boolean input parameters are often an indicator
that a method does _two_ things instead of one.

```ABAP
" anti-pattern
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

Also, method calls with a single - and thus unnamed - Boolean parameter
tend to obscure the parameter's meaning.

```ABAP
" anti-pattern
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

Splitting the method may simplify the methods' code
and describe the different intentions better

```ABAP
update_without_saving( ).
update_and_save( ).
```

Common perception suggests that setters for Boolean variables are okay:

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> Read more in
> [1](http://www.beyondcode.org/articles/booleanVariables.html)
> [2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/)
> [3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### Nombres de parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#nombres-de-parámetros)

#### Considera llamar RESULT al parámetro RETURNING

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Nombres de parámetros](#nombres-de-parámetros) > [Esta sección](#considera-llamar-result-al-parámetro-returning)

Good method names are usually so good that the `RETURNING` parameter does not need a name of its own.
The name would do little more than parrot the method name or repeat something obvious.

Repeating a member name can even produce conflicts that need to be resolved by adding a superfluous `me->`.

```ABAP
" anti-pattern
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

In these cases, simply call the parameter `RESULT`, or something like `RV_RESULT` if you prefer Hungarian notation.

Name the `RETURNING` parameter if it is _not_ obvious what it stands for,
for example in methods that return `me` for method chaining,
or in methods that create something but don't return the created entity but only its key or so.

### Inicialización de parámetros

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#inicialización-de-parámetros)

#### Clear or overwrite EXPORTING reference parameters

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Inicialización de parámetros](#inicialización-de-parámetros) > [Esta sección]Limpia o sobre-escribe parámetros de referencia EXPORTING

Reference parameters refer to existing memory areas that may be filled beforehand.
Clear or overwrite them to provide reliable data:

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

> Code inspector and Checkman point out `EXPORTING` variables that are never written.
Use these static checks to avoid this otherwise rather obscure error source.

##### [Cuida si la entrada y la salida pueden ser lo mismo]

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Inicialización de parámetros](#inicialización-de-parámetros) > [Esta sección](#cuida-si-la-entrada-y-la-salida-pueden-ser-lo-mismo)

Generally, it is a good idea to clear the parameter as a first thing in the method after type and data declarations.
This makes the statement easy to spot and avoids that the still-contained value is accidentally used by later statements.

However, some parameter configurations could use the same variable as input and output.
In this case, an early `CLEAR` would delete the input value before it can be used, producing wrong results.

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

Consider redesigning such methods by replacing `EXPORTING` with `RETURNING`.
Also consider overwriting the `EXPORTING` parameter in a single result calculation statement.
If neither fits, resort to a late `CLEAR`.

#### No hagas CLEAR a parámetros VALUE

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Inicialización de parámetros](#inicialización-de-parámetros) > [Esta sección](#no-hagas-clear-a-parámetros-value)

Parameters that work by `VALUE` are handed over as new, separate memory areas that are empty by definition.
Don't clear them again:

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

`RETURNING` parameters are always `VALUE` parameters, so you never have to clear them:

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

A method should do one thing, and only one thing.
It should do it in the best way possible.

A method likely does one thing if

- it has [few input parameters](#aim-for-few-importing-parameters-at-best-less-than-three)
- it [doesn't include Boolean parameters](#split-method-instead-of-boolean-input-parameter)
- it has [exactly one output parameter](#return-export-or-change-exactly-one-parameter)
- it is [small](#keep-methods-small)
- it [descends one level of abstraction](#descend-one-level-of-abstraction)
- it only [throws one type of exception](#throw-one-type-of-exception)
- you cannot extract meaningful other methods
- you cannot meaningfully group its statements into sections

#### Enfócate en el happy path o en manejo de errores, no en ambos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Cuerpo del método](#cuerpo-del-método) > [Esta sección](#enfócate-en-el-happy-path-o-en-manejo-de-errores-no-en-ambos)

As a specialization of the rule [_Do one thing, do it well, do it only_](#do-one-thing-do-it-well-do-it-only),
a method should either follow the happy-path it's built for,
or the error-handling-detour in case it can't,
but probably not both.

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

Can be decomposed into

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

or, to stress the validation part

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

Statements in a method should be one level of abstraction below the method itself.
Correspondingly, they should all be on the same level of abstraction.

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

instead of confusing mixtures of low level (`trim`, `to_upper`, ...) and high level (`publish`, ...) concepts like

```ABAP
" anti-pattern
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

A reliable way to find out what the right level of abstraction is is this:
Let the method's author explain what the method does in few, short words, without looking at the code.
The bullets (s)he numbers are the sub-methods the method should call or the statements it should execute.

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

The following `DATA` declaration alone is sufficient to see that the surrounding method does way more than one thing:

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

Of course there are occasions where it does not make sense to reduce a larger method further.
This is perfectly okay as long as the method remains [focused on one thing](#do-one-thing-do-it-well-do-it-only):

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

However, it still makes sense to validate whether the verbose code hides a more suitable pattern:

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> Cutting methods very small can have bad impact on performance because it increases the number of method calls.
> The [section _Mind the performance_](#mind-the-performance) gives guidance on how to balance Clean Code and performance.

### Flujo de control

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Esta sección](#flujo-de-control)

#### Falla rápido

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Flujo de control](#flujo-de-control) > [Esta sección](#falla-rápido)

Validate and fail as early as possible:

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

Later validations are harder to spot and understand and may have already wasted resources to get there.

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

There is no consensus on whether you should use `CHECK` or `RETURN` to exit a method
if the input doesn't meet expectations.

While `CHECK` definitely provides the shorter syntax

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

the statement's name doesn't reveal what happens if the condition fails,
such that people will probably understand the long form better:

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD.
```

You can avoid the question completely by reversing the validation
and adopting a single-return control flow

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD.
```

In any case, consider whether returning nothing is really the appropriate behavior.
Methods should provide a meaningful result, meaning either a filled return parameter, or an exception.
Returning nothing is in many cases similar to returning `null`, which should be avoided.

> The [section _Exiting Procedures_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)
> recommends using `CHECK` in this instance.
> Community discussion suggests that the statement is so unclear
> that many people will not understand the program's behavior.

#### Evita CHECK en otros lugares

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Métodos](#métodos) > [Flujo de control](#flujo-de-control) > [Esta sección](#evita-check-en-otros-lugares)

Do not use `CHECK` outside of the initialization section of a method.
The statement behaves differently in different positions and may lead to unclear, unexpected effects.

For example,
[`CHECK` in a `LOOP` ends the current iteration and proceeds with the next one](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm);
people might accidentally expect it to end the method or exit the loop.
Prefer using an `IF` statement in combination with `CONTINUE` instead, since `CONTINUE` only can be used in loops.

> Based on the [section _Exiting Procedures_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm).
> Note that this contradicts the [keyword reference for `CHECK` in loops](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm).

## Manejo de errores

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#manejo-de-errores)

### Mensajes

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#mensajes)

#### Haz que los mensajes sean fáciles de encontrar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Mensajes](#mensajes) > [Esta sección](#haz-que-los-mensajes-sean-fáciles-de-encontrar)

To make messages easy to find through a where-used search from transaction SE91, use the following pattern:

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

In case variable `message` is not needed, add the pragma `##NEEDED`:

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

Avoid the following:

```ABAP
" anti-pattern
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

This is an anti-pattern since:
- It contains unreachable code.
- It tests a condition which can never be true for equality.

### Códigos de retorno

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Esta sección](#códigos-de-retorno)

#### Prefiere excepciones a códigos de retorno

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Códigos de retorno](#códigos-de-retorno) > [Esta sección](#prefiere-excepciones-a-códigos-de-retorno)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

instead of

```ABAP
" anti-pattern
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

Exceptions have multiple advantages over return codes:

- Exceptions keep your method signatures clean:
you can return the result of the method as a `RETURNING` parameter and still throw exceptions alongside.
Return codes pollute your signatures with additional parameters for error handling.

- The caller doesn't have to react to them immediately.
He can simply write down the happy path of his code.
The exception-handling `CATCH` can be at the very end of his method, or completely outside.

- Exceptions can provide details on the error in their attributes and through methods.
Return codes require you to devise a different solution on your own, such as also returning a log.

- The environment reminds the caller with syntax errors to handle exceptions.
Return codes can be accidentally ignored without anybody noticing.

#### No dejes pasar las fallas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Return Codes](#códigos-de-retorno) > [Esta sección](#no-dejes-pasar-las-fallas)

If you do have to use return codes, for example because you call Functions and older code not under your control,
make sure you don't let failures slip through.

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

If something is a regular, valid case, it should be handled with regular result parameters.

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

Exceptions should be reserved for cases that you don't expect and that reflect error situations.

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

Misusing exceptions misguides the reader into thinking something went wrong, when really everything is just fine.
Exceptions are also much slower than regular code because they need to be constructed
and often gather lots of context information.

#### Usa excepciones basadas en clases

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Excepciones](#excepciones) > [Esta sección](#usa-excepciones-basadas-en-clases)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

The outdated non-class-based exceptions have the same features as return codes and shouldn't be used anymore.

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

Consider creating abstract super classes for each exception type for your application,
instead of sub-classing the foundation classes directly.
Allows you to `CATCH` all _your_ exceptions.
Enables you to add common functionality to all exceptions, such as special text handling.
`ABSTRACT` prevents people from accidentally using these non-descriptive errors directly.

#### Lanza un solo tipo de excepción

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-un-solo-tipo-de-excepción)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

In the vast majority of cases, throwing multiple types of exceptions has no use.
The caller usually is neither interested nor able to distinguish the error situations.
He will therefore typically handle them all in the same way -
and if this is the case, why distinguish them in the first place?

```ABAP
" anti-pattern
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

A better solution to recognize different error situations is using one exception type
but adding sub-classes that allow - but don't require - reacting to individual error situations,
as described in [Use sub-classes to enable callers to distinguish error situations](#use-sub-classes-to-enable-callers-to-distinguish-error-situations).

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

If there are many different error situations, use error codes instead:

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

If an exception can be expected to occur and be reasonably handled by the receiver,
throw a checked exception inheriting from `CX_STATIC_CHECK`: failing user input validation,
missing resource for which there are fallbacks, etc.

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

This exception type _must_ be given in method signatures and _must_ be caught or forwarded to avoid syntax errors.
It is therefore plain to see for the consumer and ensures that (s)he won't be surprised by an unexpected exception
and will take care of reacting to the error situation.

> This is in sync with the [ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm)
> but contradicts [_Código Limpio_ por Robert C. Martin],
> which recommends to prefer unchecked exceptions;
> [Excepciones](sub-sections/Exceptions.md) explains why.

#### Lanza CX_NO_CHECK para situaciones de las que típicamente no se puede recuperar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-cx_no_check-para-situaciones-de-las-que-típicamente-no-se-puede-recuperar)

If an exception is so severe that the receiver is unlikely to recover from it, use `CX_NO_CHECK`:
failure to read a must-have resource, failure to resolve the requested dependency, etc.

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` _cannot_ be declared in method signatures,
such that its occurrence will come as a bad surprise to the consumer.
In the case of unrecoverable situations, this is okay
because the consumer will not be able to do anything useful about it anyway.

However, there _may_ be cases where the consumer actually wants to recognize and react to this kind of failure.
For example, a dependency manager could throw a `CX_NO_CHECK` if it's unable to provide an implementation
for a requested interface because regular application code will not be able to continue.
However, there may be a test report that tries to instantiate all kinds of things just to see if it's working,
and that will report failure simply as a red entry in a list -
this service should be able to catch and ignore the exception instead of being forced to dump.

#### Considera CX_DYNAMIC_CHECK para excepciones que no se pueden evitar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#considera-cx_dynamic_check-para-excepciones-que-no-se-pueden-evitar)

Use cases for `CX_DYNAMIC_CHECK` are rare,
and in general we recommend to resort to the other exception types.
However, you may want to consider this kind of exception
as a replacement for `CX_STATIC_CHECK` if the caller has full,
conscious control over whether an exception can occur.

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

For example, consider the method `get_db_length_decs`
of class `cl_abap_math`, that tells you the number of digits
and decimal places of a decimal floating point number.
This method raises the dynamic exception `cx_parameter_invalid_type`
if the input parameter does not reflect a decimal floating point number.
Usually, this method will be called
for a fully and statically typed variable,
such that the developer knows
whether that exception can ever occur or not.
In this case, the dynamic exception would enable the caller
to omit the unnecessary `CATCH` clause.

#### Lanza un dump para situaciones que son completamente irrecuperables

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#lanza-un-dump-para-situaciones-que-son-completamente-irrecuperables)

If a situation is so severe that you are totally sure the receiver is unlikely to recover from it,
or that clearly indicates a programming error, dump instead of throwing an exception:
failure to acquire memory, failed index reads on a table that must be filled, etc.

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

This behavior will prevent any kind of consumer from doing anything useful afterwards.
Use this only if you are sure about that.

#### Prefiere RAISE EXCEPTION NEW en lugar de RAISE EXCEPTION TYPE

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Manejo de errores](#manejo-de-errores) > [Lanzamiento de excepciones](#lanzamiento-de-excepciones) > [Esta sección](#prefiere-raise-exception-new-en-lugar-de-raise-exception-type)

Note: Available from NW 7.52 onwards.

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

in general is shorter than the needlessly longer

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

However, if you make massive use of the addition `MESSAGE`, you may want to stick with the `TYPE` variant:

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

The [Law of Demeter](https://en.wikipedia.org/wiki/Law_of_Demeter) recommends de-coupling things.
Forwarding exceptions from other components violates this principle.
Make yourself independent from the foreign code by catching those exceptions
and wrapping them in an exception type of your own.

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

instead of

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

Clean Code does _not_ forbid you to comment your code - it encourages you to exploit _better_ means,
and resort to comments only if that fails.

> This example has been challenged from a performance point of view,
> claiming that cutting the methods so small worsens performance too much.
> Sample measurements show that the refactored code is 2.13 times slower than the original dirty variant.
> The clean variant takes 9.6 microseconds to fix the input `31-02-2018`, the dirty variant only 4.5 microseconds.
> This may be a problem when the method is run very often in a high-performance application;
> for regular user input validation, it should be acceptable.
> Resort to the section [Mind the performance](#mind-the-performance) to deal with Clean Code and performance issues.

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

This not only makes the intent, structure, and dependencies of the code much clearer,
it also avoids carry-over errors when temporary variables aren't properly cleared between the sections.

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

Nobody needs repeating the code in natural language

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

Nobody reads that - seriously.
If people need to read a textbook to be able to use your code,
this may be an indicator that your code has severe design issues that you should solve otherwise.
Some code _does_ need some explanation beyond a single line of comment;
consider linking the design document in these cases.

### Usa " para comentar, no *

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa--para-comentar-no-)

Quote comments indent along with the statements they comment

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

Asterisked comments tend to indent to weird places

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

Clearer than

```ABAP
" anti-pattern
output = calculate_result( input ).
" delegate pattern
```

And less invasive than

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### Borra el código en lugar de comentarlo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#borra-el-código-en-lugar-de-comentarlo)

```ABAP
" anti-pattern
* output = calculate_result( input ).
```

When you find something like this, delete it.
The code is obviously not needed because your application works and all tests are green.
Deleted code can be reproduced from the version history later on.
If you need to preserve a piece of code permanently, copy it to a file or a `$TMP` or `HOME` object.

### Usa FIXME, TODO y XXX y agrega tu usuario

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-fixme-todo-y-xxx-y-agrega-tu-usuario)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME` points to errors that are too small or too much in-the-making for internal incidents.
- `TODO`s are places where you want to complete something in the near(!) future.
- `XXX` marks code that works but could be better.

When you enter such a comment, add your nick, initials, or user to enable your co-developers to contact you
and ask questions if the comment is unclear.

### No agregues prototipos ni comentarios de fin de métodos

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#no-agregues-prototipos-ni-comentarios-de-fin-de-métodos)

Method signature comments don't help anybody.

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

Decades ago, when you couldn't see the method signature when inspecting its code,
or working with printouts that had dozens of pages, these comments may have made sense.
But all modern ABAP IDEs (SE24, SE80, ADT) show the method signature easily
such that these comments have become nothing but noise.

> In the form-based editor of SE24/SE80, press button _Signature_.
> In the ABAP Development Tools, mark the method name and press F2
> or add the view _ABAP Element Info_ to your perspective.

Similarly, end-of comments are superfluous.
These comments may have been helpful decades ago,
when programs and functions and the nested IFs inside were hundreds of lines of code long.
But our modern coding style produces methods short enough to readily see
what opening statement an `ENDIF` or `ENDMETHOD` belongs to:

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

Messages change independently from your code,
and nobody will remember adjusting the comment,
such that it will outdate and even become misleading quickly
and without anybody noticing.

The modern IDEs give you easy ways to see the text behind a message,
for example in the ABAP Development Tools,
mark the message ID and press Shift+F2.

If you want it more explicit,
consider extracting the message to a method of its own.

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### Usa ABAP Doc únicamente para APIs públicas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-abap-doc-únicamente-para-apis-públicas)

Write ABAP Doc to document public APIs,
meaning APIs that are intended for developers
in other teams or applications.
Don't write ABAP Doc for internal stuff.

ABAP Doc suffers from the same weaknesses as all comments,
that is, it outdates quickly and then becomes misleading.
As a consequence, you should employ it only where it makes sense,
not enforce writing ABAP Doc for each and everything.

> Lee más en _Capítulo 4: Good Comments: Javadocs in Public APIs_ and _Capítulo 4: Bad Comments:
> Javadocs in Nonpublic Code_ de [_Código Limpio_ por Robert C. Martin].

### Usa pragmas en lugar de pseudo-comentarios

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Comentarios](#comentarios) > [Esta sección](#usa-pragmas-en-lugar-de-pseudo-comentarios)

Prefer pragmas to pseudo comments to suppress irrelevant warnings and errors identified by the ATC. Pseudo comments 
have mostly become obsolete and have been replaced by pragmas.

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" anti-pattern
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

Use program `ABAP_SLIN_PRAGMAS` or table `SLIN_DESC` to find the mapping between obsolete pseudo comments and the pragmas that 
have replaced them.

## Formato

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#formato)

The suggestions below are [optimized for reading, not for writing](#optimize-for-reading-not-for-writing).
As ABAP's Pretty Printer doesn't cover them, some of them produce additional manual work to reformat statements
when name lengths etc. change; if you want to avoid this, consider dropping rules like
[Align assignments to the same object, but not to different ones](#align-assignments-to-the-same-object-but-not-to-different-ones).

### Sé consistente

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Esta sección](#sé-consistente)

Format all code of a project in the same way.
Let all team members use the same formatting style.

If you edit foreign code, adhere to that project's formatting style
instead of insisting on your personal style.

If you change your formatting rules over time,
use [refactoring best practices](#how-to-refactor-legacy-code)
to update your code over time.

### Optimiza para lectura, no para escritura

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#optimiza-para-lectura-no-para-escritura)

Developers spend most time _reading_ code.
Actually _writing_ code takes up a way smaller portion of the day.

As a consequence, you should optimize your code formatting for reading and debugging, not for writing.

For example, you should prefer

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

to hacks such as

```ABAP
" anti-pattern
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### Usa el Pretty Printer antes de activar

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#use-the-pretty-printer-before-activating)

Apply the pretty printer - Shift+F1 in SE80, SE24, and ADT - before activating an object.

If you modify a larger unformatted legacy code base,
you may want to apply the Pretty Printer only to selected lines
to avoid huge change lists and transport dependencies.
Consider pretty-printing the complete development object
in a separate Transport Request or Note.

> Lee más en _Capítulo 5: Formatting: Team Rules_ de [_Código Limpio_ por Robert C. Martin].

### Usa la configuración de Pretty Printer de tu equipo 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-la-configuración-de-pretty-printer-de-tu-equipo)

Always use your team settings.
Specify them under
_Menu_ > _Utilities_ > _Settings ..._ > _ABAP Editor_ > _Pretty Printer_.

Set _Indent_ and _Convert Uppercase/Lowercase_ > _Uppercase Keyword_
as agreed in your team.

> [Upper vs. Lower Case](sub-sections/UpperVsLowerCase.md) explains
> why we do not give clear guidance for the type case of keywords.
>
> Lee más en _Capítulo 5: Formatting: Team Rules_ de [_Código Limpio_ por Robert C. Martin].

### No más de una sentencia por línea de código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#no-más-de-una-sentencia-por-línea-de-código)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

Even if some occurrences may trick you into the misconception that this was readable:

```ABAP
" anti-pattern
DATA do_this TYPE i. do_this = input + 3.
```

### Mantén una longitud de línea razonable

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#mantén-una-longitud-de-línea-razonable)

Adhere to a maximum line length of 120 characters.

The human eye reads text more comfortably if the lines are not too wide -
ask a UI designer or eye movement researcher of your choice.
You will also appreciate the narrower code when debugging or comparing two sources next to each other.

The 80 or even 72 characters limit originating in the old terminal devices is a little too restrictive.
While 100 characters are often recommended and a viable choice, 120 characters seem to work a little better for ABAP,
maybe because of the general verbosity of the language.

> As a reminder you can configure in ADT the print margin to 120 characters,
> which then is visualized in the code view as a vertical line.
> Configure it under _Menu_ > _Window_ > _Preferences_ > _General_ > _Editors_ > _Text Editors_.

### Condensa tu código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#condensa-tu-código)

```ABAP
DATA(result) = calculate( items ).
```

instead of adding unneeded blanks

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

to highlight that the two statements do different things. But there is no reason for

```ABAP
" anti-pattern
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

The urge to add separating blank lines may be an indicator that your method doesn't [do one thing](#do-one-thing-do-it-well-do-it-only).

### No te obsesiones con separar usando líneas en blanco 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#no-te-obsesiones-con-separar-usando-líneas-en-blanco)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

No reason for the bad habit to tear your code apart with blank lines

```ABAP
" anti-pattern
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

Blank lines actually only make sense if you have statements that span multiple lines

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

To highlight that these things somehow belong together

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

or even better

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

But leave things ragged that have nothing to do with each other:

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> Lee más en _Capítulo 5: Formatting: Horizontal Alignment_ de [_Código Limpio_ por Robert C. Martin].

### Cierra paréntesis en la última línea de código 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#cierra-paréntesis-en-la-última-línea-de-código)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

instead of the needlessly longer

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

instead of the needlessly longer

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

When this makes the lines very long, you can break the parameters into the next line:

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

Aligning the parameters elsewhere makes it hard to spot what they belong to:

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

However, this is the best pattern if you want to avoid the formatting to be broken by a name length change.

### Usa saltos de línea para múltiples parámetros 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#usa-saltos-de-línea-para-múltiples-parámetros)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Yes, this wastes space.
However, otherwise, it's hard to spot where one parameter ends and the next starts:

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

Ragged margins make it hard to see where the parameter ends and its value begins:

```ABAP
" anti-pattern
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> This is on the other side the best pattern if you want to avoid the formatting to be broken by a name length change.

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

Indent parameter keywords by 2 spaces and parameters by 4 spaces:

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

If you have no keywords, indent the parameters by 4 spaces.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Use the Tab key to indent. It's okay if this adds one more space than needed.
(This happens if the `DATA(sum) =` part at the left has an uneven number of characters.)

### Indenta declaraciones in-line como llamadas a métodos 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Formato](#formato) > [Esta sección](#indenta-declaraciones-in-line-como-llamadas-a-métodos)

Indent in-line declarations with VALUE or NEW as if they were method calls:

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

A variable and its type belong together and should therefore be visually grouped in close proximity.
Aligning the `TYPE` clauses draws attention away from that and suggests that the variables form one vertical group, and their types another one.
Alignment also produces needless editing overhead, requiring you to adjust all indentations when the length of the longest variable name changes.

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

Write all code in a way that allows you to test it in an automatic fashion.

If this requires refactoring your code, do it.
Do that first, before you start adding other features.

If you add to legacy code that is too badly structured to be tested,
refactor it at least to the extent that you can test your additions.

#### Permite que otros hagan mock de tu código 

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#permite-que-otros-hagan-mock-de-tu-código)

If you write code to be consumed by others, enable them to write unit tests for their own code,
for example by adding interfaces in all outward-facing places,
providing helpful test doubles that facilitate integration tests,
or applying dependency inversion to enable them to substitute the productive configuration with a test config.

#### Reglas de legibilidad  

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#reglas-de-legibilidad)

Make your test code even more readable than your productive code.
You can tackle bad productive code with good tests, but if you don't even get the tests, you're lost.

Keep your test code so simple and stupid that you will still understand it in a year from now.

Stick to standards and patterns, to enable your co-workers to quickly get into the code.

#### No hagas copias ni escribas reportes de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#no-hagas-copias-ni-escribas-reportes-de-prueba)

Don't start working on a backlog item by making a `$TMP` copy of a development object and playing around with it.
Others won't notice these objects and therefore won't know the status of your work.
You will probably waste a lot of time by making the working copy in the first place.
You will also forget to delete the copy afterwards, spamming your system and dependencies.
(Don't believe this? Go to your development system and check your `$TMP` right now.)

Also, don't start by writing a test report that calls something in a specific way,
and repeat that to verify that things are still working when you're working on it.
This is poor man's testing: repeating a test report by hand and verifying by eye whether everything is still fine.
Take the next step and automate this report in a unit test,
with an automatic assertion that tells you whether the code is still okay.
First, you will spare yourself the effort of having to write the unit tests afterwards.
Second, you will save a lot of time for the manual repetitions, plus avoid getting bored and tired over it.

#### Prueba componentes públicos, no los privados

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#prueba-componentes-públicos-no-los-privados)

Public parts of classes, especially the interfaces they implement, are rather stable and unlikely to change.
Let your unit tests validate only the publics to make them robust
and minimize the effort you have to spend when you refactor the class.
Protected and private internals, in contrast, may change very quickly through refactoring,
such that each refactoring would needlessly break your tests.

An urgent need to test private or protected methods may be an early warning sign for several kinds of design flaws.
Ask yourself:

- Did you accidentally bury a concept in your class that wants to come out into its own class,
with its own dedicated suite of tests?

- Did you forget to separate the domain logic from the glue code?
For example, implementing the domain logic directly in the class that is plugged into BOPF as an action,
determination, or validation, or that was generated by SAP Gateway as a `*_DPC_EXT` data provider, may not the best idea.

- Are your interfaces too complicated and request too much data that is irrelevant or that cannot be mocked easily?

#### No te obsesiones con la cobertura del código

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Principios](#principios) > [Esta sección](#no-te-obsesiones-con-la-cobertura-del-código)

Code coverage is there to help you find code you forgot to test, not to meet some random KPI:

Don't make up tests without or with dummy asserts just to reach the coverage.
Better leave things untested to make transparent that you cannot safely refactor them.
You can have < 100% coverage and still have perfect tests.
There are cases - such as IFs in the constructor to insert test doubles -
that may make it unpractical to reach 100%.
Good tests tend to cover the same statement multiple times, for different branches and conditions.
They will in fact have imaginary > 100% coverage.

### Clases de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#clases-de-prueba)

#### Llama las clases locales de prueba de acuerdo a su objetivo

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Clases de prueba](#clases-de-prueba) > [Esta sección](#llama-las-clases-locales-de-prueba-de-acuerdo-a-su-objetivo)

Name local test classes either by the "when" part of the story

```ABAP
CLASS ltc_<public method name> DEFINITION FOR TESTING ... ."
```

or the "given" part of the story

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

Put unit tests into the local test include of the class under test.
This ensures that people find these tests when refactoring the class
and allows them to run all associated tests with a single key press,
as described in [How to execute test classes](#how-to-execute-test-classes).

Put component-, integration- and system tests into the local test include of a separate global class.
They do not directly relate to a single class under test, therefore they should not arbitrarily be
placed in one of the involved classes, but in a separate one.  
Mark this global test class as `FOR TESTING` and `ABSTRACT`
to avoid that it is accidentally referenced in production code.  
Putting tests into other classes has the danger that people overlook them
and forget to run them when refactoring the involved classes.

Therefore it is beneficial to use *test relations* to document which objects
are tested by the test.  
With the example below the test class `hiring_test`
could be executed while being in the class `recruting` or `candidate` via the shrotcut `Shift-Crtl-F12` (Windows) or `Cmd-Shift-F12` (macOS).

```abap
"! @testing recruting
"! @testing candidate
class hiring_test definition
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### Coloca métodos de ayuda en clases de ayuda

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Clases de prueba](#clases-de-prueba) > [Esta sección](#coloca-métodos-de-ayuda-en-clases-de-ayuda)

Put help methods used by several test classes in a help class. Make the help methods available through 
inheritance (is-a relationship) or delegation (has-a relationship).

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

In the ABAP Development Tools, press Ctrl+Shift+F10 to run all tests in a class.
Press Ctrl+Shift+F11 to include coverage measurements.
Press Ctrl+Shift+F12 to also run tests in other classes that are maintained as test relations.

> On macOS, use `Cmd` instead of `Ctrl`.

### Código bajo prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#código-bajo-prueba)

#### Nombra el código bajo prueba con sentido, o usa CUT como default

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Código Bajo Prueba](#código-bajo-prueba) > [Esta sección](#nombra-el-código-bajo-prueba-con-sentido-o-usa-cut-como-default)

Give the variable that represents the code under test a meaningful name:

```ABAP
DATA blog_post TYPE REF TO ...
```

Don't just repeat the class name with all its non-valuable namespaces and prefixes:

```ABAP
" anti-pattern
DATA clean_fra_blog_post TYPE REF TO ...
```

If you have different test setups, it can be helpful to describe the object's varying state:

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

If you have problems finding a meaningful name, resort to `cut` as a default.
The abbreviation stands for "code under test".

```ABAP
DATA cut TYPE REF TO ...
```

Especially in unclean and confusing tests, calling the variable `cut`
can temporarily help the reader see what's actually tested.
However, tidying up the tests is the actual way to go for the long run.

#### Prueba sobre interfaces, no implementaciones

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Código Bajo Prueba](#código-bajo-prueba) > [Esta sección](#prueba-sobre-interfaces-no-implementaciones)

A practical consequence of the [_Test publics, not private internals_](#test-publics-not-private-internals),
type your code under test with an _interface_

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

rather than a _class_

```ABAP
" anti-pattern
DATA code_under_test TYPE REF TO some_class.
```

#### Extrae la llamada al código bajo prueba a su propio método

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Código Bajo Prueba](#código-bajo-prueba) > [Esta sección](#extrae-la-llamada-al-código-bajo-prueba-a-su-propio-método)

If the method to be tested requires a lot of parameters or prepared data,
it can help to extract the call to it to a helper method of its own that defaults the uninteresting parameters:

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

Calling the original method directly can swamp your test with a lot of meaningless details:

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

Dependency inversion means that you hand over all dependencies to the constructor:

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

Don't use setter injection.
It enables using the productive code in ways that are not intended:

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

Don't use FRIENDS injection.
It will initialize productive dependencies before they are replaced, with probably unexpected consequences.
It will break as soon as you rename the internals.
It also circumvents initializations in the constructor.

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

Shorter and easier to understand than custom test doubles:

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

In general, a clean programming style
will let you do much of the work
with standard ABAP unit tests and test doubles.
However, there are tools that will allow you
to tackle trickier cases in elegant ways:

- Use the `CL_OSQL_REPLACE` service
to test complex OpenSQL statements
by redirecting them to a test data bin
that can be filled with test data
without interfering with the rest of the system.

- Use the CDS test framework to test your CDS views.

#### Usa test seams como una solución temporal

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#usa-test-seams-como-una-solución-temporal)

If all other techniques fail, or when in dangerous shallow waters of legacy code,
refrain to [test seams](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abaptest-seam.htm)
to make things testable.

Although they look comfortable at first sight, test seams are invasive and tend to get entangled
in private dependencies, such that they are hard to keep alive and stable in the long run.

We therefore recommend to refrain to test seams only as a temporary workaround
to allow you refactoring the code into a more testable form.

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

Unit tests that access private and protected members to insert mock data are fragile:
they break when the internal structure of the tested code changes.

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

Don't sub-class and overwrite methods to mock them in your unit tests.
Although this works, it is fragile because the tests break easily when refactoring the code.
It also enables real consumers to inherit your class,
which [may hit you unprepared when not explicitly designing for it](#final-if-not-designed-for-inheritance).

```ABAP
" anti-pattern
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

To get legacy code under test,
[resort to test seams instead](#use-test-seams-as-temporary-workaround).
They are just as fragile but still the cleaner way because they at least don't change the class's productive behavior,
as would happen when enabling inheritance by removing a previous `FINAL` flag or by changing method scope from `PRIVATE` to `PROTECTED`.

When writing new code, take this testability issue into account directly when designing the class,
and find a different, better way.
Common best practices include [resorting to other test tools](#exploit-the-test-tools)
and extracting the problem method to a separate class with its own interface.

> A more specific variant of
> [Don't change the productive code to make the code testable](#dont-change-the-productive-code-to-make-the-code-testable).

#### No hagas mock sin necesidad

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-hagas-mock-sin-necesidad)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

Define your givens as precisely as possible: don't set data that your test doesn't need,
and don't mock objects that are never called.
These things distract the reader from what's really going on.

```ABAP
" anti-pattern
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

There are also cases where it's not necessary to mock something at all -
this is usually the case with data structures and data containers.
For example, your unit tests may well work with the productive version of a `transient_log`
because it only stores data without any side effects.

#### No crees librerías para pruebas

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Inyección](#inyección) > [Esta sección](#no-crees-librerías-para-pruebas)

Unit tests - in contrast to integration tests - should be data-in-data-out, with all test data being defined on the fly as needed.

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

Don't start building frameworks that distinguish "*test case IDs*" to decide what data to provide.
The resulting code will be so long and tangled that you won't be able to keep these tests alive in the long term.

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

Good names reflect the given and then of the test:

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

Bad names reflect the when, repeat meaningless facts, or are cryptic:

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

As ABAP allows only 30 characters in method names, it's fair to add an explanatory comment
if the name is too short to convey enough meaning.
ABAP Doc or the first line in the test method may be an appropriate choice for the comment.

Having lots of test methods whose names are too long may be an indicator
that you should split your single test class into several ones
and express the differences in the givens in the class's names.

#### Usa dado-cuando-entonces

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#usa-dado-cuando-entonces)

Organize your test code along the given-when-then paradigm:
First, initialize stuff in a given section ("given"),
second call the actual tested thing ("when"),
third validate the outcome ("then").

If the given or then sections get so long
that you cannot visually separate the three sections anymore, extract sub-methods.
Blank lines or comments as separators may look good at first glance
but don't really reduce the visual clutter.
Still they are helpful for the reader and the novice test writer to separate the sections.

#### "Cuando" es exactamente una llamada

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#cuando-es-exactamente-una-llamada)

Make sure that the "when" section of your test method contains exactly one call to the class under test:

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Calling multiple things indicates that the method has no clear focus and tests too much.
This makes it harder to find the cause when the test fails:
was it the first, second, or third call that caused the failure?
It also confuses the reader because he is not sure what the exact feature under test is.

#### No uses el método TEARDOWN a menos que realmente lo necesites

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Métodos de prueba](#métodos-de-prueba) > [Esta sección](#no-uses-el-método-teardown-a-menos-que-realmente-lo-necesites)

`teardown` methods are usually only needed to clear up database entries
or other external resources in integration tests.

Resetting members of the test class, esp. `cut` and the used test doubles, is superfluous;
they are overwritten by the `setup` method before the next test method is started.

### Datos de prueba

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Esta sección](#datos-de-prueba)

#### Haz que sea fácil detectar la intención

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Datos de prueba](#datos-de-prueba) > [Esta sección](#haz-que-sea-fácil-detectar-la-intención)

In unit tests, you want to be able to quickly tell which data and doubles are important,
and which ones are only there to keep the code from crashing.
Support this by giving things that have no meaning obvious names and values, for example:

```ABAP
DATA(alert_id) = '42'.                             " well-known meaningless numbers
DATA(detection_object_type) = '?=/"&'.             " 'keyboard accidents'
CONSTANTS some_random_number TYPE i VALUE 782346.  " revealing variable names
```

Don't trick people into believing something connects to real objects or real customizing if it doesn't:

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

Don't force readers to compare long meaningless strings to spot tiny differences.

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

Assert only exactly what the test method is about, and this with a small number of assertions.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Asserting too much is an indicator that the method has no clear focus.
This couples productive and test code in too many places: changing a feature
will require rewriting a large number of tests although they are not really involved with the changed feature.
It also confuses the reader with a large variety of assertions,
obscuring the one important, distinguishing assertion among them.

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

Asserts often do more than meets the eye, for example `assert_equals`
includes type matching and providing precise descriptions if values differ.
Using the wrong, too-common asserts will force you into the debugger immediately
instead of allowing you to see what is wrong right from the error message.

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

Don't write magic-number-quantity assertions if you can express the actual contenido you expect.
Numbers may vary although the expectations are still met.
In reverse, the numbers may match although the contenido is something completely unexpected.

```ABAP
" anti-pattern
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### Haz aserciones sobre la calidad, no el contenido

> [Clean ABAP](#clean-abap) > [Contenido](#contenido) > [Testing](#testing) > [Aserciones](#aserciones) > [Esta sección](#haz-aserciones-sobre-la-calidad-no-el-contenido)

If you are interested in a meta quality of the result,
but not in the actual contenido itself, express that with a suitable assert:

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

Asserting the precise contenido obscures what you actually want to test.
It is also fragile because refactoring may produce a different
but perfectly acceptable result although it breaks all your too-precise unit tests.

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

Your test code remains focused on the happy path and is therefore much easier to read and understand, as compared to:

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

Instead of copy-pasting this over and over again.
