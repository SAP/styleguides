> Translated from [English original on 14.11.2019](https://github.com/SAP/styleguides/tree/72ecf7fd7d41151d5bbca29020d4ec9de953db8c).
> Latest version [in English](CleanABAP.md).

# ABAP 整洁之道

> [**中文**](CleanABAP_zh.md)
> &nbsp;·&nbsp;
> [English](CleanABAP.md)
> &nbsp;·&nbsp;
> [Français](CleanABAP_fr.md)
> &nbsp;·&nbsp;
> [Deutsch](CleanABAP_de.md)

本指南针对 [ABAP](https://zh.wikipedia.org/wiki/ABAP) 改编自 [Robert C. Martin 所著的 _Clean Code_]。

[速查表](cheat-sheet/CheatSheet.md)为打印优化版本。

[Robert C. Martin 所著的 _Clean Code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## 目录

- [做法](#how-to)
   - [整洁代码入门之法](#how-to-get-started-with-clean-code)
   - [旧代码重构之法](#how-to-refactor-legacy-code)
   - [自动检查之法](#how-to-check-automatically)
   - [与其他指南互通之法](#how-to-relate-to-other-guides)
   - [表示异议之法](#how-to-disagree)
- [名称](#names)
   - [使用描述性名称](#use-descriptive-names)
   - [首选解决方案域和问题域术语](#prefer-solution-domain-and-problem-domain-terms)
   - [使用复数形式](#use-plural)
   - [使用能读出来的名称](#use-pronounceable-names)
   - [避免缩写](#avoid-abbreviations)
   - [在各处使用相同缩写](#use-same-abbreviations-everywhere)
   - [用名词表示类而用动词表示方法](#use-nouns-for-classes-and-verbs-for-methods)
   - [避免干扰词，如 "data"、"info"、"object"](#avoid-noise-words-such-as-data-info-object)
   - [每个概念选取一个词](#pick-one-word-per-concept)
   - [仅在本意如此时使用模式名称](#use-pattern-names-only-if-you-mean-them)
   - [避免编码，特别是匈牙利表示法和前缀](#avoid-encodings-esp-hungarian-notation-and-prefixes)
- [语言](#language)
   - [顾及传统](#mind-the-legacy)
   - [顾及性能](#mind-the-performance)
   - [面向对象编程优于过程式编程](#prefer-object-orientation-to-procedural-programming)
   - [函数式语言结构优于过程式语言结构](#prefer-functional-to-procedural-language-constructs)
   - [避免过时语言元素](#avoid-obsolete-language-elements)
   - [明智地使用设计模式](#use-design-patterns-wisely)
- [常量](#constants)
   - [使用常量而非幻数](#use-constants-instead-of-magic-numbers)
   - [枚举类优于常量接口](#prefer-enumeration-classes-to-constants-interfaces)
   - [如果不使用枚举类，则对常量进行分组](#if-you-dont-use-enumeration-classes-group-your-constants)
- [变量](#variables)
   - [内联声明优于最前声明](#prefer-inline-to-up-front-declarations)
   - [勿在可选分支中内联声明](#dont-declare-inline-in-optional-branches)
   - [勿用链式最前声明](#do-not-chain-up-front-declarations)
   - [REF TO 优于 FIELD-SYMBOL](#prefer-ref-to-to-field-symbol)
- [表](#tables)
   - [使用恰当的表类型](#use-the-right-table-type)
   - [避免 DEFAULT KEY](#avoid-default-key)
   - [INSERT INTO TABLE 优于 APPEND TO](#prefer-insert-into-table-to-append-to)
   - [LINE_EXISTS 优于 READ TABLE 或 LOOP AT](#prefer-line_exists-to-read-table-or-loop-at)
   - [READ TABLE 优于 LOOP AT](#prefer-read-table-to-loop-at)
   - [LOOP AT WHERE 优于嵌套式 IF](#prefer-loop-at-where-to-nested-if)
   - [避免不必要的表读取](#avoid-unnecessary-table-reads)
- [字符串](#strings)
   - [使用 ` 定义文字](#use--to-define-literals)
   - [使用 | 汇集文本](#use--to-assemble-text)
- [布尔值](#booleans)
   - [明智地使用布尔值](#use-booleans-wisely)
   - [用 ABAP_BOOL 表示布尔值](#use-abap_bool-for-booleans)
   - [使用 ABAP_TRUE 和 ABAP_FALSE 进行比较](#use-abap_true-and-abap_false-for-comparisons)
   - [使用 XSDBOOL 设置布尔变量](#use-xsdbool-to-set-boolean-variables)
- [条件](#conditions)
   - [尽量使条件为正](#try-to-make-conditions-positive)
   - [IS NOT 优于 NOT IS](#prefer-is-not-to-not-is)
   - [考虑分解复杂条件](#consider-decomposing-complex-conditions)
   - [考虑提炼复杂条件](#consider-extracting-complex-conditions)
- [If 语句](#ifs)
   - [无空的 IF 分支](#no-empty-if-branches)
   - [对于多个备选条件，CASE 优于 ELSE IF](#prefer-case-to-else-if-for-multiple-alternative-conditions)
   - [保持低嵌套深度](#keep-the-nesting-depth-low)
- [正则表达式](#regular-expressions)
   - [较简单的方法优于正则表达式](#prefer-simpler-methods-to-regular-expressions)
   - [基本检查优于正则表达式](#prefer-basis-checks-to-regular-expressions)
   - [考虑汇集复杂的正则表达式](#consider-assembling-complex-regular-expressions)
- [类](#classes)
   - [类：面向对象](#classes-object-orientation)
      - [对象优于静态类](#prefer-objects-to-static-classes)
      - [组合优于继承](#prefer-composition-to-inheritance)
      - [勿在同一个类中混用有态和无态](#dont-mix-stateful-and-stateless-in-the-same-class)
   - [作用域](#scope)
      - [缺省情况下为全局，仅在适当位置为局部](#global-by-default-local-only-where-appropriate)
      - [若非为继承而设计则为 FINAL](#final-if-not-designed-for-inheritance)
      - [缺省情况下为 PRIVATE，仅在需要时为 PROTECTED](#members-private-by-default-protected-only-if-needed)
      - [考虑使用不可变对象而非 getter](#consider-using-immutable-instead-of-getter)
      - [保守地使用 READ-ONLY](#use-read-only-sparingly)
   - [构造函数](#constructors)
      - [NEW 优于 CREATE OBJECT](#prefer-new-to-create-object)
      - [如果全局类为 CREATE PRIVATE，则保留 CONSTRUCTOR 为公共](#if-your-global-class-is-create-private-leave-the-constructor-public)
      - [多个静态创建方法优于可选参数](#prefer-multiple-static-creation-methods-to-optional-parameters)
      - [用描述性名称表示多个创建方法](#use-descriptive-names-for-multiple-creation-methods)
      - [仅在多实例无意义的情况下变成单例](#make-singletons-only-where-multiple-instances-dont-make-sense)
- [方法](#methods)
   - [调用](#calls)
      - [函数式调用优于过程式调用](#prefer-functional-to-procedural-calls)
      - [省略 RECEIVING](#omit-receiving)
      - [省略可选关键字 EXPORTING](#omit-the-optional-keyword-exporting)
      - [在单参数调用中省略参数名称](#omit-the-parameter-name-in-single-parameter-calls)
      - [在调用实例方法时省略自我引用 me](#omit-the-self-reference-me-when-calling-an-instance-method)
   - [方法：面向对象](#methods-object-orientation)
      - [实例优于静态方法](#prefer-instance-to-static-methods)
      - [公共实例方法应为接口的一部分](#public-instance-methods-should-be-part-of-an-interface)
   - [参数数目](#parameter-number)
      - [力图减少 IMPORTING 参数，最好少于三个](#aim-for-few-importing-parameters-at-best-less-than-three)
      - [拆分方法而非添加 OPTIONAL 参数](#split-methods-instead-of-adding-optional-parameters)
      - [保守地使用 PREFERRED PARAMETER](#use-preferred-parameter-sparingly)
      - [RETURN、EXPORT 或 CHANGE 恰有一个参数](#return-export-or-change-exactly-one-parameter)
   - [参数类型](#parameter-types)
      - [RETURNING 优于 EXPORTING](#prefer-returning-to-exporting)
      - [RETURNING 大表通常没有问题](#returning-large-tables-is-usually-okay)
      - [单独使用 RETURNING 或 EXPORTING 或 CHANGING，而不要组合使用](#use-either-returning-or-exporting-or-changing-but-not-a-combination)
      - [在合适时保守地使用 CHANGING](#use-changing-sparingly-where-suited)
      - [拆分方法而非使用布尔输入参数](#split-method-instead-of-boolean-input-parameter)
   - [参数名称](#parameter-names)
      - [考虑调用 RETURNING 参数 RESULT](#consider-calling-the-returning-parameter-result)
   - [参数初始化](#parameter-initialization)
      - [清除或覆盖 EXPORTING 引用参数](#clear-or-overwrite-exporting-reference-parameters)
         - [如果输入和输出可能相同则要当心](#take-care-if-input-and-output-could-be-the-same)
      - [勿清除 VALUE 参数](#dont-clear-value-parameters)
   - [方法体](#method-body)
      - [做且仅做一件事，把它做好](#do-one-thing-do-it-well-do-it-only)
      - [关注愉快路径或错误处理，但非两者兼顾](#focus-on-the-happy-path-or-error-handling-but-not-both)
      - [将抽象降一级](#descend-one-level-of-abstraction)
      - [保持方法精简](#keep-methods-small)
   - [控制流](#control-flow)
      - [快速失败](#fail-fast)
      - [CHECK 对 RETURN](#check-vs-return)
      - [避免在其他位置使用 CHECK](#avoid-check-in-other-positions)
- [错误处理](#error-handling)
   - [消息](#messages)
      - [使消息易于查找](#make-messages-easy-to-find)
   - [返回代码](#return-codes)
      - [异常优于返回代码](#prefer-exceptions-to-return-codes)
      - [别让故障溜走](#dont-let-failures-slip-through)
   - [异常](#exceptions)
      - [异常针对的是错误，而不是正常情况](#exceptions-are-for-errors-not-for-regular-cases)
      - [使用基于类的异常](#use-class-based-exceptions)
   - [抛出](#throwing)
      - [使用各自的超类](#use-own-super-classes)
      - [抛出一种类型的异常](#throw-one-type-of-exception)
      - [使用子类以便调用者能够区分错误情况](#use-sub-classes-to-enable-callers-to-distinguish-error-situations)
      - [针对可应对的异常抛出 CX_STATIC_CHECK](#throw-cx_static_check-for-manageable-exceptions)
      - [针对通常不可恢复的情况抛出 CX_NO_CHECK](#throw-cx_no_check-for-usually-unrecoverable-situations)
      - [针对可避免的异常考虑 CX_DYNAMIC_CHECK](#consider-cx_dynamic_check-for-avoidable-exceptions)
      - [针对完全不可恢复的情况进行转储](#dump-for-totally-unrecoverable-situations)
      - [RAISE EXCEPTION NEW 优于 RAISE EXCEPTION TYPE](#prefer-raise-exception-new-to-raise-exception-type)
   - [捕获](#catching)
      - [包裹外来异常而非任其侵入代码](#wrap-foreign-exceptions-instead-of-letting-them-invade-your-code)
- [注释](#comments)
   - [用代码表达自己而不是靠注释](#express-yourself-in-code-not-in-comments)
   - [注释绝非坏名称的借口](#comments-are-no-excuse-for-bad-names)
   - [使用方法而非注释来对代码分段](#use-methods-instead-of-comments-to-segment-your-code)
   - [写注释是要解释为什么而非是什么](#write-comments-to-explain-the-why-not-the-what)
   - [设计应放到设计文档里而不是代码里](#design-goes-into-the-design-documents-not-the-code)
   - [用 " 而非 * 加注释](#comment-with--not-with-)
   - [将注释放在与其相关的语句前面](#put-comments-before-the-statement-they-relate-to)
   - [删除代码而非将其注释掉](#delete-code-instead-of-commenting-it)
   - [使用 FIXME、TODO 和 XXX 并添加自己的标识](#use-fixme-todo-and-xxx-and-add-your-id)
   - [勿添加方法签名和注释结尾](#dont-add-method-signature-and-end-of-comments)
   - [勿复制消息文本作为注释](#dont-duplicate-message-texts-as-comments)
   - [ABAP 文档仅适用于公共 API](#abap-doc-only-for-public-apis)
   - [编译指示优于伪注释](#prefer-pragmas-to-pseudo-comments)
- [格式化](#formatting)
   - [保持一致](#be-consistent)
   - [为阅读而不是书写进行优化](#optimize-for-reading-not-for-writing)
   - [激活前使用格式优化器](#use-the-pretty-printer-before-activating)
   - [使用格式优化器团队设置](#use-your-pretty-printer-team-settings)
   - [每行只有一条语句](#no-more-than-one-statement-per-line)
   - [恪守合理的行长度](#stick-to-a-reasonable-line-length)
   - [紧缩代码](#condense-your-code)
   - [添加单一空行来分隔内容，而不要添加多行](#add-a-single-blank-line-to-separate-things-but-not-more)
   - [勿因分隔空行产生困扰](#dont-obsess-with-separating-blank-lines)
   - [对齐同一对象而非不同对象的赋值](#align-assignments-to-the-same-object-but-not-to-different-ones)
   - [在行尾关闭括号](#close-brackets-at-line-end)
   - [保持单参数调用于一行](#keep-single-parameter-calls-on-one-line)
   - [保持参数在调用后面](#keep-parameters-behind-the-call)
   - [如果换行，则在调用下缩进参数](#if-you-break-indent-parameters-under-the-call)
   - [将多个参数换行](#line-break-multiple-parameters)
   - [对齐参数](#align-parameters)
   - [如果调用行过长则将其换行](#break-the-call-to-a-new-line-if-the-line-gets-too-long)
   - [缩进并卡到制表位](#indent-and-snap-to-tab)
   - [如同方法调用那样缩进内联声明](#indent-in-line-declarations-like-method-calls)
   - [勿对齐类型子句](#dont-align-type-clauses)
- [测试](#testing)
   - [原则](#principles)
      - [编写可测试的代码](#write-testable-code)
      - [让他人能够进行模拟](#enable-others-to-mock-you)
      - [可读性规则](#readability-rules)
      - [勿制作副本或写测试报告](#dont-make-copies-or-write-test-reports)
      - [测试公共项而非私有内部项](#test-publics-not-private-internals)
      - [勿困扰于覆盖范围](#dont-obsess-about-coverage)
   - [测试类](#test-classes)
      - [按用途调用局部测试类](#call-local-test-classes-by-their-purpose)
      - [将测试放在局部类](#put-tests-in-local-classes)
      - [将帮助方法放在帮助类](#put-help-methods-in-help-classes)
      - [如何执行测试类](#how-to-execute-test-classes)
   - [被测代码](#code-under-test)
      - [赋予被测代码有意义的名称，或使用缺省名称 CUT](#name-the-code-under-test-meaningfully-or-default-to-cut)
      - [测试接口而非类](#test-interfaces-not-classes)
      - [将被测代码的调用提取到自身的方法](#extract-the-call-to-the-code-under-test-to-its-own-method)
   - [注入](#injection)
      - [使用依赖倒置注入测试替身](#use-dependency-inversion-to-inject-test-doubles)
      - [考虑使用 ABAP 测试替身工具](#consider-to-use-the-tool-abap-test-double)
      - [利用测试工具](#exploit-the-test-tools)
      - [使用测试缝隙作为临时解决办法](#use-test-seams-as-temporary-workaround)
      - [使用 LOCAL FRIENDS 访问依赖倒置的构造函数](#use-local-friends-to-access-the-dependency-inverting-constructor)
      - [勿滥用 LOCAL FRIENDS 侵入被测代码](#dont-misuse-local-friends-to-invade-the-tested-code)
      - [勿更改生产代码来使代码可测试](#dont-change-the-productive-code-to-make-the-code-testable)
      - [勿子类化来模拟方法](#dont-sub-class-to-mock-methods)
      - [勿模拟不需要的东西](#dont-mock-stuff-thats-not-needed)
      - [勿构建测试框架](#dont-build-test-frameworks)
   - [测试方法](#test-methods)
      - [测试方法名称：反映出设想和预期的情形](#test-method-names-reflect-whats-given-and-expected)
      - [使用 given-when-then](#use-given-when-then)
      - ["When" 恰为一个调用](#when-is-exactly-one-call)
      - [除非真正需要否则勿添加 TEARDOWN](#dont-add-a-teardown-unless-you-really-need-it)
   - [测试数据](#test-data)
      - [使其易于辨明含义](#make-it-easy-to-spot-meaning)
      - [使其易于辨明差异](#make-it-easy-to-spot-differences)
      - [使用常量描述测试数据的用途和重要性](#use-constants-to-describe-purpose-and-importance-of-test-data)
   - [断言](#assertions)
      - [少而精的断言](#few-focused-assertions)
      - [使用恰当的断言类型](#use-the-right-assert-type)
      - [断言内容而非数量](#assert-content-not-quantity)
      - [断言质量而非内容](#assert-quality-not-content)
      - [使用 FAIL 检查是否出现预期异常](#use-fail-to-check-for-expected-exceptions)
      - [转发意外异常而非捕获就失败](#forward-unexpected-exceptions-instead-of-catching-and-failing)
      - [编写自定义断言以缩短代码和避免重复](#write-custom-asserts-to-shorten-code-and-avoid-duplication)

## 做法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#how-to)

### 整洁代码入门之法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [做法](#how-to) > [本节](#how-to-get-started-with-clean-code)

如果您初识整洁代码，应首先阅读 [Robert C. Martin 所著的 _Clean Code_]。借助 [Clean Code Developer initiative](https://clean-code-developer.com/)，您可以从头学起，循序渐进地对该主题有一般性的了解。

建议从容易理解且广为接受的方面入手，如[布尔值](#booleans)、[条件](#conditions)和 [If 语句](#ifs)。

您可能将会从[方法](#methods)一节获得最大受益，特别是[做且仅做一件事，把它做好](#do-one-thing-do-it-well-do-it-only)和[方法精简](#keep-methods-small)，因为这些会极大地改善代码的总体结构。

对于有行事经验但初识整洁代码的团队，本文的某些主题可能会引起团队内激烈的讨论；这些主题绝对“有益健康”，但人们可能刚开始不太适应。

后面会再继续探讨这些颇具争议的主题，特别是[注释](#comments)、[名称](#names)和[格式化](#formatting)，它们可能会引起孜孜不倦的争论，只有认识到整洁代码积极效应的团队才知道它的好处。

### 旧代码重构之法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [做法](#how-to) > [本节](#how-to-refactor-legacy-code)

如果正在遗留项目上工作，其中含有大量无法或不想更改的代码，因为它们可以无冲突地运行在新代码环境，这种情况下， 更改[布尔值](#booleans)、[条件](#conditions)、[If 语句](#ifs)和[方法](#methods)方面的主题最有价值。

对于遗留项目而言， [名称](#names) 主题改进太费劲了，它可能会在新旧代码之间产生差异，在某种程度上，其中的诸如[避免编码，特别是匈牙利表示法和前缀](#avoid-encodings-esp-hungarian-notation-and-prefixes)等节忽略为宜。

我们发现采用四步计划进行重构，结果比较好：

1. 先让团队上道。沟通并解释新的风格，使项目团队的每个人对此达成一致意见。不用一下子就推行所有指导原则，只需从小部分没有争议的子集入手，然后由此拓展。

2. 按照_童子军规则_开展每日的例行工作：_每次修改代码都比原先更整洁_。不要因此而困扰，好几个小时沉湎于“清理整个营地”，只需花几分钟，思考如何持续不断地改进。

3. 构筑_整洁小岛_：时不时挑选小的对象或组件，试着进行全方位的清洁。这些小岛印证了现在所做事情的好处，为进一步重构形成了经得起考验的坚强堡垒。

4. 谈经论道。不管是设立老派的[范根代码评审](https://en.wikipedia.org/wiki/Fagan_inspection)，还是举办宣讲会，抑或是在自己喜爱的聊天工具中组建讨论板：需要讲出自己的经验和体会，以使团队逐渐达成共识。

### 自动检查之法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [做法](#how-to) > [本节](#how-to-check-automatically)

没有一整套全面的静态代码检查方法可以自动检测本文所述的我们这里所描述的反面模式。

ABAP 测试主控室、代码分析器、扩展检查和检查管理器提供了一些检查方法，这些方法可能有助于发现某些问题。

[abapOpenChecks](https://github.com/larshp/abapOpenChecks) 是一个开源的代码分析器检查集，也涵盖了所述的某些反面模式。

[abaplint](https://github.com/abaplint/abaplint) 是 一个ABAP 解析器的开源的实现重写。它不需要SAP系统就可以运行，旨在用 abapGit 使代码串行化。它提供了多个集成（GitHub Actions、Jenkins、文本编辑器...），涵盖了某些反面模式，也可用来检查格式化和代码规范。

### 与其他指南互通之法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [做法](#how-to) > [本节](#how-to-relate-to-other-guides)

本指南秉承整洁代码的_精神_，这意味着我们对 ABAP 编程语言进行了一些调整，例如，[针对可管理的异常抛出 CX_STATIC_CHECK](#throw-cx_static_check-for-manageable-exceptions)。

某些论据来自 [ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm) 与本指南大多是兼容的；背离之处予以指明，务求符合整洁代码的精神。

本指南也遵循 [DSAG's Recommendations for ABAP Development](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf)，不过我们在大多数细节上更加精确。

### 表示异议之法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [做法](#how-to) > [本节](#how-to-disagree)

编写本风格指南的目标读者已通晓整洁代码或目前正致力于此，且对如何将整洁代码_具体应用于 ABAP_ 极为关注。

因此，请注意，我们没有以原书同样的篇幅和深度介绍所有概念及相关资源：那些内容仍值得一读，特别是，如果您只是因为我们没解释太详细而不同意本文的观点。可使用各节中的链接延伸阅读我们给出指导的背景。

您尽可以讨论文本讲述的任何内容并表示异议。整洁代码的支柱之一是_团队规则_。在您放弃异议之前，一定要给它们一个公平的机会。

[CONTRIBUTING.md](../CONTRIBUTING.md) 就如何变通本指南或在小的细节上另辟蹊径，给出了建议。

## 名称

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#names)

### 使用描述性名称

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#use-descriptive-names)

使用可以传达事物内容和含义的名称。

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

不要只把注意力放在数据类型和技术编码上。它们对理解代码几乎没什么贡献。

```ABAP
" anti-pattern
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[不要试图通过注释来弥补坏的名称。](#comments-are-no-excuse-for-bad-names)

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Use Intention-Revealing Names_。

### 首选解决方案域和问题域术语

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#prefer-solution-domain-and-problem-domain-terms)

在解决方案域（即计算机科学术语，如 "queue" 或 "tree"）和问题域（即业务领域术语，如 "account" 或 "ledger"）中搜索好的名称。

按问题域命名时，业务层的命名最好听。对于采用域驱动设计而设计的组件（如 API 和业务对象）尤为如此。

按解决方案域命名时，提供大多数技术功能（如工厂类和抽象算法）层的命名最好听。

在任何情况下都不要试图加进自己的语言。需能够在开发人员、产品负责人、合作伙伴和客户之间交换信息，因此要选择所有人不用查定制词典就能理解的名称。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Use Solution Domain Names_ and _[...]: > Use Problem Domain Names_。

### 使用复数形式

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#use-plural)

在 SAP 有一种传统习惯，那就是用单数形式命名事物的表，例如，`country` 表示“国家表”。外界普遍倾向于使用复数形式表示事物的列表。因此，建议最好改用 `countries`。

> 这条建议主要针对诸如变量和属性等事物。> 对于开发对象，可能存在同样> 也有意义的模式，例如，有一种广泛使用的规范，> 以单数形式命名数据库表（“透明表”）。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Use Intention-Revealing Names_。

### 使用能读出来的名称

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#use-pronounceable-names)

关于对象会有很多思考和讨论，因此要使用能读出来的名称，例如，`detection_object_types` 优于诸如 `dobjt` 这种晦涩的名称。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Use Pronounceable Names_。

### 避免缩写

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#avoid-abbreviations)

如果有足够空间，那就完整地写出名称。仅当超过长度限制时才使用缩写。

如果不得不缩写，首先考虑_不重要_的词。

采用缩写，可能第一眼看起来很高效，但很快就会变得含糊不清。例如，`cust` 中的 "cust" 究竟是指 "customizing"、"customer" 还是 "custom"？三者在 SAP 应用程序中都很常见。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Make Meaningful Distinctions_。

### 在各处使用相同缩写

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#use-same-abbreviations-everywhere)

人们会搜索关键字来查找相关代码。为此，应对相同事物使用相同缩写。例如，始终将 "detection object type" 缩写为 "dobjt"，而不是混合使用 "dot"、"dotype"、"detobjtype" 等等。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Use Searchable Names_。

### 用名词表示类而用动词表示方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#use-nouns-for-classes-and-verbs-for-methods)

使用名词或名词词组命名类、接口和对象：

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

使用动词或动词词组命名方法：

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

用诸如 `is_` 和 `has_` 之类的动词作为布尔方法的开头，读起来会很流畅：

```ABAP
IF is_empty( table ).
```

建议也像方法一样给函数命名：

```ABAP
FUNCTION /clean/read_alerts
```

### 避免干扰词，如 "data"、"info"、"object"

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#avoid-noise-words-such-as-data-info-object)

省略干扰词

```ABAP
account  " instead of account_data
alert    " instead of alert_object
```

或将其替换为某些确实更有价值的特定字眼

```ABAP
user_preferences          " instead of user_info
response_time_in_seconds  " instead of response_time_variable
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Make Meaningful Distinctions_

### 每个概念选取一个词

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#pick-one-word-per-concept)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

为一个概念选择一个术语并坚持使用；不要混合使用其他同义词。同义词会使读者浪费时间查找本不存在的差异。

```ABAP
" anti-pattern
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Pick One Word per Concept_

### 仅在本意如此时使用模式名称

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#use-pattern-names-only-if-you-mean-them)

不要对类和接口使用软件设计模式的名称，除非本意真的如此。例如，不要将类称为 `file_factory`，除非它的确实施了工厂设计模式。最常见的模式包括：[singleton](https://en.wikipedia.org/wiki/Singleton_pattern)、[factory](https://en.wikipedia.org/wiki/Factory_method_pattern)、[facade](https://en.wikipedia.org/wiki/Facade_pattern)、[composite](https://en.wikipedia.org/wiki/Composite_pattern)、[decorator](https://en.wikipedia.org/wiki/Decorator_pattern)、[iterator](https://en.wikipedia.org/wiki/Iterator_pattern)、[observer](https://en.wikipedia.org/wiki/Observer_pattern) 和 [strategy](https://en.wikipedia.org/wiki/Strategy_pattern)。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 2: Meaningful Names: Avoid Disinformation_

### 避免编码，特别是匈牙利表示法和前缀

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [名称](#names) > [本节](#avoid-encodings-esp-hungarian-notation-and-prefixes)

鼓励丢掉_所有_编码前缀。

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

而不是毫无必要地加长

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> [Avoid Encodings](sub-sections/AvoidEncodings.md) > 深入介绍了这样做的理由。

## 语言

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#language)

### 顾及传统

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [语言](#language) > [本节](#mind-the-legacy)

如果是针对较早的 ABAP 版本进行编码，则应谨慎采纳本指南中的建议：下文的许多建议利用了相对较新的语法和结构，这些在较早的 ABAP 版本中可能不受支持。在必须支持的最早版本上验证欲遵循的指导原则。不要简单地整个抛弃整洁代码 - 绝大多数规则（例如，命名、注释）在_任何_ ABAP 版本中都行得通。

### 顾及性能

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [语言](#language) > [本节](#mind-the-performance)

如果是为高性能组件编码，则应谨慎采纳本指南中的建议：整洁代码在某些方面可能会降低速度（更多方法调用）或消耗更多内存（更多对象）。ABAP 的某些特点可能会加剧这种情况，例如，在调用方法时，它会比较数据类型，这样一来，将单个大方法拆分成多个子方法，可能会降低代码速度。

然而，强烈建议不要因为模糊的恐惧就过早地悲观失望。绝大多数规则（例如，命名、注释）根本不会产生任何负面影响。尽力采用整洁的面向对象的方式做事情。如果有什么过慢，就做一个性能测量。只有这样做之后，才应根据事实作出决策，放弃所选规则。

一些更深入的思考，部分取自 [Martin Fowler 所著的 _Refactoring_](https://martinfowler.com/books/refactoring.html) 中的第 2 章：

在典型的应用程序中，大部分运行时间都花在很小比例的代码中。小到 10% 的代码会占到 90% 的运行时间，特别是在 ABAP 中，很大比例的运行时间可能都是数据库时间。

因此，花大力气试图使_所有_代码都一直保持超高效率，并非最好的资源安排方式。不主张忽视性能，但在初始开发阶段，应该更关注代码的整洁性和条理分明的程度，然后使用剖析器找出关键区域进行优化。

事实上，我们有理由证明，这种方式对性能的正面影响更大，因为优化努力更有针对性，更容易找出性能瓶颈，而且条理分明的代码更容易进行重构和调优。

### 面向对象编程优于过程式编程

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [语言](#language) > [本节](#prefer-object-orientation-to-procedural-programming)

面向对象的程序（类、接口）比过程式代码（函数、程序）分段更清晰，并且可以更加容易地进行重构和测试。尽管在某些情况下必须提供过程式对象（对 RFC 用函数、对事务用程序），但这些对象除了调用提供实际功能的相应类之外，不应该再干别的：

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> [Function Groups vs. Classes](sub-sections/FunctionGroupsVsClasses.md) > 详细描述了两者的差异。

### 函数式语言结构优于过程式语言结构

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [语言](#language) > [本节](#prefer-functional-to-procedural-language-constructs)

它们通常更加简短，而且更容易为现代程序员所接受。

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

下文的许多详细规则只不过是具体重申了这条通用的建议。

### 避免过时语言元素

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [语言](#language) > [本节](#avoid-obsolete-language-elements)

在升级 ABAP 版本时，务必要检查是否有过时的语言元素，避免再使用它们。

例如，以下语句中 `@` 转义的 "host" 变量更清楚地表明了什么是程序变量、什么是数据库中的列，

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

相较于[过时的转义形式](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm)

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

较新的可选方案倾向于提高代码的可读性，减少与现代编程范式的设计冲突，这样切换到这些方案时就会自动使代码更整洁。

如果继续使用旧代码编写方式，过时元素可能在处理速度和内存消耗方面无法再从优化中受益。

使用现代语言元素，可以更轻松地将年轻的 ABAP 程序员带上道，由于在 SAP 的培训中不再教授过时内容，他们可能不再熟悉过时的结构。

SAP NetWeaver 文档固定包含一部分，其中列出了过时的语言元素，例如，[NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm)、[NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm)、[NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm)、[NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm)。

### 明智地使用设计模式

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [语言](#language) > [本节](#use-design-patterns-wisely)

仅在合适且有明显好处的地方使用。不要为了使用而到处用设计模式。

## 常量

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#constants)

### 使用常量而非幻数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [常量](#constants) > [本节](#use-constants-instead-of-magic-numbers)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

在清晰方面好于

```ABAP
" anti-pattern
IF abap_type = 'D'.
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 17: Smells and Heuristics: G25: > Replace Magic Numbers with Named Constants_。

### 枚举类优于常量接口

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [常量](#constants) > [本节](#prefer-enumeration-classes-to-constants-interfaces)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

或

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

而不是将不相关的东西混在一起

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

> [Enumerations](sub-sections/Enumerations.md) > 描述了常见的枚举模式> 并讨论了它们的优缺点。
> 
> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 17: Smells and Heuristics: J3: Constants versus Enums_。

### 如果不使用枚举类，则对常量进行分组

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [常量](#constants) > [本节](#if-you-dont-use-enumeration-classes-group-your-constants)

如果以松散方式集合常量，例如，在接口中，则应将其分组：

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

使关系更清晰，好于：

```ABAP
" Anti-pattern
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

利用组还可以成组进行访问，例如，进行输入验证：

```ABAP
DO number_of_constants TIMES.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(<constant>).
  IF <constant> = input.
    is_valid = abap_true.
    RETURN.
  ENDIF.
ENDWHILE.
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 17: Smells and Heuristics: G27: Structure over Convention_。

## 变量

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#variables)

### 内联声明优于最前声明

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [变量](#variables) > [本节](#prefer-inline-to-up-front-declarations)

如果遵循本文的指导原则，在首次出现的地方内联式声明变量显得更加自然，方法体也会变得很精短（3-5 条语句）。

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

好过在方法开头单独的 `DATA` 部分声明变量

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

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 5: Formatting: Vertical Distance: Variable Declarations_。

### 勿在可选分支中内联声明

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [变量](#variables) > [本节](#dont-declare-inline-in-optional-branches)

```ABAP
" anti-pattern
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

这样可以正常运行，因为 ABAP 会像声明位于方法开头那样来处理内联式声明。然而，这会令读者感到极其迷惑，特别是方法体较长而又没当场发现声明的话。在此情况下，不要使用内联式声明而将声明放在最前面：

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 5: Formatting: Vertical Distance: Variable Declarations_。

### 勿用链式最前声明

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [变量](#variables) > [本节](#do-not-chain-up-front-declarations)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /dirty/reader.
```

链式处理主张在逻辑层级关联定义的变量。为了一致性，必须确保所有链式变量结成一体，要添加变量，就得另外引入链组。尽管这种方法可行，但通常不值得花这个功夫。

另外，链式处理也毫无必要地使重新格式化和重构变得复杂，因为每行看起来都不同，改起来需要四处挪动冒号、句号和逗号，根本不值得花功夫。

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO /dirty/reader.
```

> 另请参阅 [Don't align type clauses](#dont-align-type-clauses)
> 如果使用链式数据声明，则每组结成一体的变量各用一个链。

### REF TO 优于 FIELD-SYMBOL

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [变量](#variables) > [本节](#prefer-ref-to-to-field-symbol)

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

而非等效形式

```ABAP
" anti-pattern
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

需要指针的地方除外

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

从代码评审的经验表明人们往往会随心所欲地做出选择：“就是因为”、“因为我们总是采用那种循环方式”，或者“没特殊原因”。随意选择会令读者把时间浪费在毫无意义的问题上：为什么用这个而不用那个，因此应代之以有理有据、准确无误的决策。我们的建议基于这种理由：

- 指针能做一些引用做不了的事情，比如动态访问结构的组成部分。同样，引用也能做指针做不了的事情，比如构造动态类型的数据结构。总之，单独指望一个是不行的。

- 在面向对象的 ABAP 中，引用到处都有并且无法避免，因为任何对象皆是 `REF TO <class-name>`。相反，指针仅在涉及动态类型的少数特殊情况下才绝对需要。因此，引用自然成为任何面向对象程序中的首选。

- 指针比引用短，但结果节省的内存却微不足道，尽可以忽略不计。同样，速度也不是问题。因此，在性能方面没理由厚此薄彼。

> 更多信息参阅 > [ABAP Programming Guidelines 中的篇章 _Accessing Data Objects Dynamically_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm)。

## 表

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#tables)

### 使用恰当的表类型

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#use-the-right-table-type)

- `HASHED` 表通常用来表示**单步填充**、**永不修改**且**常按键值读取**的**大表**。其固有的内存和处理开销使得散列表仅在数据量很大且读访问次数很多的情况下才有价值。每次对表内容进行更改，均需要大量重新计算散列值，因此修改过于频繁的表不要使用此种类型。

- `SORTED` 表通常用于表示需要**时时排序**、**逐位填充**或**需要修改**并且**常按一个或多个完整或部分键值读取**或**以某种特定顺序**处理的**大表**。添加、更改或移除内容，需要找到恰当的插入点，但不需要调整表索引的其余部分。仅对读访问次数很多的情况，有序表才有价值。

- `STANDARD` 表用于表示索引开销大于索引受益的**小表**，以及或是毫不在乎行顺序或是就想完全按追加顺序进行处理的**“数组”**。另外，也适用于需要对表进行不同访问的情况，例如，通过 `SORT` 和 `BINARY SEARCH` 进行索引访问和排序访问。

> 这些只是粗略的指导原则。> 更多细节参见 [ABAP Language Help 中的篇章 _Selection of Table Category_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm)。

### 避免 DEFAULT KEY

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#avoid-default-key)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

添加缺省键值常常只是为了让具有较新功能的语句得以正常工作。事实上，这些键值本身通常是多余的，除了耗费资源，别无它用。由于它们会忽略数值数据类型，因此甚至可能会导致隐蔽的错误。不含显式字段列表的 `SORT` 和 `DELETE ADJACENT` 语句将会转而采用内部表的主键，在使用 `DEFAULT KEY` 的情况下，这可能会导致十分意想不到的结果，例如，当以数值字段作为键值的分量时，特别是当与 `READ TABLE ... BINARY` 等结合使用时。

要么显式指定键值

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

如果根本不需要键值的话，则采用 `EMPTY KEY`。。

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> 参照 [Horst Keller 的博客文章 _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)
> **注意：**具有 `EMPTY KEY` 的内部表上的 `SORT` 根本不会进行排序，> 但假如能静态确定键值为空，就会发出语法警告。

### INSERT INTO TABLE 优于 APPEND TO

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#prefer-insert-into-table-to-append-to)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` 对所有表和键值类型都起作用，因而更便于在性能需求发生变化时重构表的类型和键值定义。

仅当以类似数组的方式使用 `STANDARD` 表时才使用 `APPEND TO`，如果想要强调所添加的条目应为最后一行的话。

### LINE_EXISTS 优于 READ TABLE 或 LOOP AT

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#prefer-line_exists-to-read-table-or-loop-at)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

更清楚简洁地表明意图，好于

```ABAP
" anti-pattern
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

或者甚至是

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### READ TABLE 优于 LOOP AT

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#prefer-read-table-to-loop-at)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

更清楚简洁地表明意图，好于

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

或者甚至是

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### LOOP AT WHERE 优于嵌套式 IF

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#prefer-loop-at-where-to-nested-if)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

更清楚简洁地表明意图，好于

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### 避免不必要的表读取

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [表](#tables) > [本节](#avoid-unnecessary-table-reads)

若你_预期_某一行就在表里，那就读取一次并对异常作出处理就够了，

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

而不是用两次读取打乱并减慢主控制流

```ABAP
" anti-pattern
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
DATA(row) = my_table[ key = input ].
```

> 除了提高性能以外，这还是更一般性的[关注愉快路径或错误处理，但非两者兼顾](#focus-on-the-happy-path-or-error-handling-but-not-both)的一种特殊变化形式。

## 字符串

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#strings)

### 使用 ` 定义文字

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [字符串](#strings) > [本节](#use--to-define-literals)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

避免使用 `'`，因为它会增加多余的类型转换，并且会令读者困惑于处理的究竟是 `CHAR` 还是 `STRING`：

```ABAP
" anti-pattern
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` 一般都适用，但无法用于 `CONSTANTS`，而且在指定固定值时会增加不必要的开销：

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### 使用 | 汇集文本

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [字符串](#strings) > [本节](#use--to-assemble-text)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

字符串模板更加突出地表明何为文字、何为变量，特别是如果在文本中嵌入多个变量的话。

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## 布尔值

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#booleans)

### 明智地使用布尔值

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [布尔值](#booleans) > [本节](#use-booleans-wisely)

经常会遇到下面这种情况，布尔值似乎是自然的选择

```ABAP
" anti-pattern
is_archived = abap_true.
```

而换个视角才发现本应选择枚举

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

一般来说，用布尔值区分事物的类型是一种坏的选择，因为几乎总会遇到并非彼此排斥的情况

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

此外，[拆分方法而非使用布尔输入参数](#split-method-instead-of-boolean-input-parameter)还解释了为何应始终回避布尔参数。

> 更多信息参阅 [1](http://www.beyondcode.org/articles/booleanVariables.html)

### 用 ABAP_BOOL 表示布尔值

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [布尔值](#booleans) > [本节](#use-abap_bool-for-booleans)

```ABAP
DATA has_entries TYPE abap_bool.
```

不要使用普通类型 `char1`。尽管在技术上兼容，但它会掩盖处理的是布尔变量这个事实。

也要避免其他布尔类型，因为它们常常会产生奇怪的副作用，例如，`boolean` 支持第三个值 "undefined"，它会导致难以觉察的编程错误。

在某些情况下，例如，对于 DynPro 字段，可能需要数据字典元素。此时无法使用 `abap_bool`，因为它是在类型池 `abap` 中而不是在数据字典中定义的。在此情况下，转而采用 `boole_d` 或 `xfeld`。如果需要自定义描述，那就创建自己的数据元素。

> ABAP 可能是唯一不带通用布尔数据类型的编程语言。然而，设立一个是大势所趋。本建议基于 ABAP Programming Guidelines。

### 使用 ABAP_TRUE 和 ABAP_FALSE 进行比较

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [布尔值](#booleans) > [本节](#use-abap_true-and-abap_false-for-comparisons)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

不要使用等效字符 `'X'` 和 `' '` 或 `space`；用它们很难看出这是一个布尔表达式：

```ABAP
" anti-pattern
has_entries = 'X'.
IF has_entries = space.
```

避免与 `INITIAL` 进行比较 - 这会迫使读者去回想 `abap_bool` 的缺省值为 `abap_false`：

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> ABAP 可能是唯一不带表示真假的内置“常量”的编程语言。然而，设立它们是大势所趋。本建议基于 ABAP Programming Guidelines。

### 使用 XSDBOOL 设置布尔变量

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [布尔值](#booleans) > [本节](#use-xsdbool-to-set-boolean-variables)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

等效的 `IF`-`THEN`-`ELSE` 除了长得多之外，别无它用：

```ABAP
" anti-pattern
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool` 是最合乎本来目的的方法，因为它直接产生 `char1`，该类型最适合布尔类型 `abap_bool`。等效函数 `boolc` 和 `boolx` 会产生不同的类型并增加不必要的隐式类型转换。

我们同意名称 `xsdbool` 不巧会产生误导；毕竟，我们对 "xsd" 前缀暗示的 "XML Schema Definition" 部分毫无兴趣。

`xsdbool` 的一种可行的备选方案是 `COND` 三元形式。其语法直接明了，但是有点长，因为它会不必要地重复 `THEN abap_true` 段，而且还需要知道隐式缺省值 `abap_false` - 这就是为什么我们建议只将其作为第二解决方案。

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## 条件

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#conditions)

### 尽量使条件为正

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [条件](#conditions) > [本节](#try-to-make-conditions-positive)

```ABAP
IF has_entries = abap_true.
```

反之，比较时看看同样的语句会变得多难理解：

```ABAP
" anti-pattern
IF has_no_entries = abap_false.
```

节标题中的“尽量”意味着事先不用强行这样做，直到在某一点要以诸如[空的 IF 分支](#no-empty-if-branches)之类的语句结束时才应如此：

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 17: Smells and Heuristics: G29: Avoid Negative Conditionals_。

### IS NOT 优于 NOT IS

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [条件](#conditions) > [本节](#prefer-is-not-to-not-is)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

否定在逻辑上是等效的，但需要“脑筋转弯”，从而加大了理解难度。

```ABAP
" anti-pattern
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> [尽量使条件为正](#try-to-make-conditions-positive)的一个更加具体的变化形式。另请参见 ABAP programming guidelines 中的 [Alternative Language Constructs](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenalternative_langu_guidl.htm) 一节。

### 考虑分解复杂条件

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [条件](#conditions) > [本节](#consider-decomposing-complex-conditions)

将条件分解成若干基本组成部分，条件就会变得更加简单：

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

而不是全都掺和在一起：

```ABAP
" anti-pattern
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> 使用 ABAP 开发工具的快速修复功能，可以很快提取条件并创建如上所示的变量。

### 考虑提炼复杂条件

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [条件](#conditions) > [本节](#consider-extracting-complex-conditions)

将复杂条件提炼成各自的方法是一个好主意：

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

## If 语句

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#ifs)

### 无空的 IF 分支

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [If 语句](#ifs) > [本节](#no-empty-if-branches)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

更加简明，好于

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### 对于多个备选条件，CASE 优于 ELSE IF

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [If 语句](#ifs) > [本节](#prefer-case-to-else-if-for-multiple-alternative-conditions)

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

采用 `CASE` 更容易看出来是一组互斥的选择。它比一连串 `IF` 执行起来更快，因为它可以转化为另一种不同的微处理器命令，而不是一连串顺序评估的条件。不必到处重复判别变量，就可以快速引入新的情况。该语句甚至可以防止无意中嵌套 `IF`-`ELSEIF` 时可能出现的一些错误。

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

### 保持低嵌套深度

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [If 语句](#ifs) > [本节](#keep-the-nesting-depth-low)

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

嵌套的 `IF` 不仅难于快速理解，而且需要指数级的测试用例才能完全覆盖。

通常可以通过形成子方法并引入辅助布尔变量来拆分决策树。

其他情况可以通过合并 IF 进行简化，比如

```ABAP
IF <this> AND <that>.
```

而不是毫无必要地嵌套

```ABAP
" anti-pattern
IF <this>.
  IF <that>.
```

## 正则表达式

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#regular-expressions)

### 较简单的方法优于正则表达式

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [正则表达式](#regular-expressions) > [本节](#prefer-simpler-methods-to-regular-expressions)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

正则表达式难以快速理解。没有它们，简单情况通常反而更加容易。

正则表达式通常也会消耗更多内存和处理时间，因为需要将其解析成表达式树并在运行时编译成可执行的匹配程序。直接使用循环和临时变量，简单就可以解决。

### 基本检查优于正则表达式

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [正则表达式](#regular-expressions) > [本节](#prefer-basis-checks-to-regular-expressions)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

而不用费事改成

```ABAP
" anti-pattern
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> 当正则表达式无处不在时，，对不重复自己 (DRY) 的原则视而不见似乎变成一种自然的倾向，请对照 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 17: Smells and Heuristics: General: G5: Duplication_。

### 考虑汇集复杂的正则表达式

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [正则表达式](#regular-expressions) > [本节](#consider-assembling-complex-regular-expressions)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

有一些复杂的正则表达式，当您向读者展示它们是如何从更基本的片段构成时，就会变得更加容易。

## 类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#classes)

### 类：面向对象

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [本节](#classes-object-orientation)

#### 对象优于静态类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [类：面向对象](#classes-object-orientation) > [本节](#prefer-objects-to-static-classes)

首先，静态类失去了面向对象所具备的全部优势。特别是，有了它们，几乎无法在单元测试中用测试替身替换生产中的相关依赖。

如果您在考虑是否该使类或方法变成静态的，答案几乎总是：不。

对于这条规则，有一种例外情况可以接受，那就是简单的实用工具类。其方法使其更容易与某些 ABAP 类型进行交互。它们不仅完全无态，而且相当初级，看起来就像是 ABAP 语句或内置函数。辨别因素是，其调用 者会将它们紧紧捆绑到各自的代码中，从而真的没法在单元测试中对其进行模拟。

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

#### 组合优于继承

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [类：面向对象](#classes-object-orientation) > [本节](#prefer-composition-to-inheritance)

避免构建具有继承性的类层次结构，应该选择组合。

很难设计出完美的继承，因为需要遵守规则，如 [Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle)。另外，也很难理解，因为人们需要认识并领会层次结构背后的指导原则。继承会降低重用性，因为方法往往仅对子类才可用。它还会使重构复杂化，因为移动或更改成员往往需要对整个层次结构树进行更改。

组合意味着要设计小的独立对象，每个对象只服务于一个特定目的。通过简单的代理和外观模式，就可以将这些对象重新组合成更复杂的对象。组合可能会产生更多的类，但除此之外再无其他缺点。

莫因这条规则而丧失在恰当之处使用继承的信心。有一些应用场合很适合使用继承，例如，[Composite design pattern](https://en.wikipedia.org/wiki/Composite_pattern)。只需中肯地问问自己，在所处情况下，继承是否确实利大于弊。如有怀疑，一般来说，选择组合更稳妥。

> [Interfaces vs. abstract classes](sub-sections/InterfacesVsAbstractClasses.md) 对此做了一些详细比较。

#### 勿在同一个类中混用有态和无态

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [类：面向对象](#classes-object-orientation)

不要在同一个类中混用无态和有态编程范式。

在无态编程中，方法获取输入并产生输出，_而不会有任何副作用_，因此无论何时、以何顺序调用，方法都会产生相同的结果。

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

在有态编程中，通过对象的方法操控其内部状态，这意味着_满是副作用_。

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

两种范式都不错，有各自的应用场合。然而，在同一个对象中_混用_会使代码难以理解，并且由于携带着的隐蔽错误以及同步性问题，注定会失败。切勿这样做。

### 作用域

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [本节](#scope)

#### 缺省情况下为全局，仅在适当位置为局部

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [作用域](#scope) > [本节](#global-by-default-local-only-where-appropriate)

默认情况下运用全局类。只有在适当位置使用局部类。

> 全局类在数据字典中可见。局部类存在于另一个开发对象的 include 内，仅对这个另外的对象可见。

局部类适用

- 用于非常特定的私有数据结构，例如全局类数据的迭代器，仅此处需要这些数据结构，

- 用于提取复杂的私有部分算法，例如从其余类代码算法中提取出特殊用途的多方法的排序聚合算法，

- 用于模拟全局类的特定方面，例如，通过将所有数据库访问提取到可在单元测试中使用测试替身替换的单独局部类。

局部类将阻碍重用，因为它们无法在其他位置使用。尽管局部类易于提取，但人们通常甚至无法找到它们，从而导致不希望的代码重复。在极长的局部类中进行定向、导航和调试非常乏味且令人讨厌。由于 ABAP 锁是在包含文件级别上的，人们将无法同时在本地包含文件的不同部分上工作（只有在它们是不同的全局类的情况下，才能执行此操作）。

在以下情况下，重新考虑局部类的使用：

- 您的本地包含文件可以包含数十个类和数千行代码，
- 您将全局类视为包含其他类的“包”，
- 您的全局类退化为空壳，
- 您发现单独的本地包含文件中有重复代码，
- 您的开发人员开始互相锁定，无法并行工作，
- 由于您的团队无法理解彼此的本地子树，因此您的工作项估计会变得很多。

#### 若非为继承而设计则为 FINAL

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [作用域](#scope) > [本节](#final-if-not-designed-for-inheritance)

将并非针对继承而明确设计的类构建为 `FINAL`。

在设计类的合作能力时，您的首选应该是[组合而不是继承](#prefer-composition-to-inheritance)。实现继承不是一件容易的事，因为需要您考虑 `PROTECTED` 与 `PRIVATE` 等属性以及 [Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle)，并且冻结了许多设计内部功能。如果您在类设计中没有考虑这些问题，那么应该通过将类构建为 `FINAL` 来防止意外继承。

当然，继承_有_一些很好的应用程序，例如设计模式[复合](https://en.wikipedia.org/wiki/Composite_pattern)。通过允许使用子类，业务加载项也可以变得更加有用，客户能够重用大多数原始代码。但是，请注意，所有这些情况下，从一开始就通过设计内置了继承。

未[实施接口](#public-instance-methods-should-be-part-of-an-interface)的不整洁类应保持非 `FINAL`，这样使用者才能在单元测试中对其进行模拟。

#### 缺省情况下为 PRIVATE，仅在需要时为 PROTECTED

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [作用域](#scope) > [本节](#members-private-by-default-protected-only-if-needed)

默认情况下，将属性、方法和其他类成员设置为 `PRIVATE`。

只有在您要启用子类覆盖它们时才将它们设置为 `PROTECTED`。

只有需要的情况下，才应让类的内部元素供其他成员或程序使用。这不仅包括外部调用者，还包括子类。信息过度可用可能会因意外重新定义而导致细微错误，并阻碍重构，因为外部调用将冻结原本应流动的成员。

#### 考虑使用不可变对象而非 getter

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [作用域](#scope) > [本节](#consider-using-immutable-instead-of-getter)

不可变对象是在构造后永不改变的对象。对于此类对象，请考虑使用公有只读属性而不是 getter 方法。

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

而不是

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

> **警告**：对于**具有**变化值的对象，请勿使用公有只读属性。否则，此属性必须始终保持最新状态，无论其他任何代码是否需要它们的值。

#### 保守地使用 READ-ONLY

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [作用域](#scope) > [本节](#use-read-only-sparingly)

许多现代编程语言（尤其是 Java）建议尽量将类成员设置为只读，以防止产生意外的不良影响。

尽管 ABAP _确实_为数据声明提供了 `READ-ONLY` 加载项，但我们建议您谨慎使用。

首先，仅在 `PUBLIC SECTION` 中提供了加载项，从而大大降低了其适用范围。您既不能将其添加到受保护的成员或私有成员，也不能将其添加到方法中的局部变量。

其次，加载项的执行结果与人们对其他编程语言的期望行为略有不同：仍然可以通过类本身、其友元类及其子类中的任何方法自由地修改 READ-ONLY 数据。这与其他语言中普遍采用的“一次写入，永远不会修改”行为相矛盾。这种差异可能会导致令人惊讶的意外。

> 为了避免误解：保护变量以防意外修改是一种很好的做法。如果是一个合适的语句，我们也建议将其应用于 ABAP中。

### 构造函数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [本节](#constructors)

#### NEW 优于 CREATE OBJECT

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [构造函数](#constructors) > [本节](#prefer-new-to-create-object)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

当然，除非需要动态类型

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### 如果全局类为 CREATE PRIVATE，则保留 CONSTRUCTOR 为公有

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [构造函数](#constructors) > [本节](#if-your-global-class-is-create-private-leave-the-constructor-public)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

我们同意这是自相矛盾的。但根据文章 [ABAP 帮助的 _Instance Constructor_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm)，需要在 `PUBLIC SECTION` 中指定 `CONSTRUCTOR` 以确保正确的编译和语法验证。

这仅适用于全局类。在局部类中，应将构造函数设置为私有。

#### 多个静态创建方法优于可选参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [构造函数](#constructors) > [本节](#prefer-multiple-static-factory-methods-to-optional-parameters)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP 不支持[过载](https://en.wikipedia.org/wiki/Function_overloading)。使用名称变式而不是可选参数来实现所需的语义。

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

[_拆分方法而非添加 OPTIONAL 参数_](#split-methods-instead-of-adding-optional-parameters)通用指南介绍了根本原因。

考虑使用[构建器设计模式](https://en.wikipedia.org/wiki/Builder_pattern)将复杂构造解析为多步构造。

#### 用描述性名称表示多个创建方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [构造函数](#constructors) > [本节](#use-descriptive-names-for-multiple-creation-methods)

用于创建方法的有效单词为 `new_`、`create_` 和 `construct_`。人们凭直觉就可以将它们与对象构造联系起来。还可以将这些单词连用构成动词短语，如 `new_from_template`、`create_as_copy` 或 `create_by_name`。

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

而不是无意义的名称，例如

```ABAP
" anti-pattern
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### 仅在多实例无意义的情况下变成单例

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [类](#classes) > [构造函数](#constructors) > [本节](#make-singletons-only-where-multiple-instances-dont-make-sense)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

在您的面向对象的设计中提到第二个实例没有意义的情况下应用单例模式。该模式可确保每个使用者都以相同的状态和相同的数据处理相同的内容。

不要出于习惯或者因为某些性能规则的评分而使用单例模式。该模式最容易被过度使用和错误应用，这会产生意想不到的交叉影响，并为测试增加不必要的复杂性。如果单一对象没有设计驱动原因，可由使用者自己做决定，他仍然可以通过构造函数之外的方式（例如使用工厂）达到相同目的。

## 方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#methods)

这些规则可应用于类和功能模块中的方法。

### 调用

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#calls)

#### 函数式调用优于过程式调用

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [调用](#calls) > [本节](#prefer-functional-to-procedural-calls)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

如果动态类型禁止函数调用，请使用过程式调用

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

以下许多详细规则只是此建议的更具体变式。

#### 省略 RECEIVING

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [调用](#calls) > [本节](#omit-receiving)

```ABAP
DATA(sum) = aggregate_values( values ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### 省略可选关键字 EXPORTING

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [调用](#calls) > [本节](#omit-the-optional-keyword-exporting)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### 在单参数调用中省略参数名称

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [调用](#calls) > [本节](#omit-the-parameter-name-in-single-parameter-calls)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates( list = list ).
```

但是，在某些情况下，仅方法名称还不够清楚，重复参数名称可能会更易于理解：

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### 在调用实例方法时省略自我引用 me

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [调用](#calls) > [本节](#omit-the-self-reference-me-when-calling-an-instance-method)

由于自我引用 `me->` 是由系统隐式设置的，因此可在调用实例方法时将其省略

```ABAP
DATA(sum) = aggregate_values( values ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
DATA(sum) = me->aggregate_values( values ).
```

### 方法：面向对象

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#methods-object-orientation)

#### 实例优于静态方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [方法：面向对象](#methods-object-orientation) > [本节](#prefer-instance-to-static-methods)

缺省情况下，方法应为实例成员。实例方法可以更好地反映类的“对象本质”。在单元测试中可以更轻松地模拟这些方法。

```ABAP
METHODS publish.
```

方法仅在特殊情况下才是静态的，例如静态创建方法。

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### 公共实例方法应为接口的一部分

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [方法：面向对象](#methods-object-orientation) > [本节](#public-instance-methods-should-be-part-of-an-interface)

公有实例方法应始终是接口的一部分。这样可以解耦相关性，并简化单元测试中的模拟过程。

```ABAP
METHOD /clean/blog_post~publish.
```

在面向整洁对象的方法中，公开没有接口的方法没有多大意义，枚举类等少数方法除外，这些方法永远不会有备选实施，也永远不会在测试用例中进行模拟。

> [接口与抽象类](sub-sections/InterfacesVsAbstractClasses.md)描述了为什么这也适用于覆盖继承方法的类。

### 参数数目

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#parameter-number)

#### 力图减少 IMPORTING 参数，最好少于三个

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数数目](#parameter-number) > [本节](#aim-for-few-importing-parameters-at-best-less-than-three)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

整洁程度优于

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

输入参数过多会大幅增加方法的复杂性，因为方法需要处理指数级的组合。有多个参数就说明该方法做了超过一件事。

您可以通过结构和对象将参数组合为有意义的集合，从而减少参数的数目。

#### 拆分方法而非添加 OPTIONAL 参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数数目](#parameter-number) > [本节](#split-methods-instead-of-adding-optional-parameters)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

为了实现ABAP不支持但又需要的语义[过载](https://en.wikipedia.org/wiki/Function_overloading)。

```ABAP
" anti-pattern
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

可选参数使调用者感到困惑：

- 真正需要哪些参数？
- 哪些参数组合有效？
- 哪些参数互相排斥？

按特定的参数拆分成多个方法可以给人明确的指引，从而避免了这种混淆。

#### 保守地使用 PREFERRED PARAMETER

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数数目](#parameter-number) > [本节](#use-preferred-parameter-sparingly)

额外定义`PREFERRED PARAMETER` 让人很难知道实际需要提供什么样的参数，理解代码也变得更困难。将参数数目减至最少，尤其是可选参数的数目，可以自动减少对 `PREFERRED PARAMETER` 的需求。

#### RETURN、EXPORT 或 CHANGE 恰有一个参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数数目](#parameter-number) > [本节](#return-export-or-change-exactly-one-parameter)

好的方法只做_一件事_，而这也应该反映在方法恰好只返回一个值。如果方法的输出参数相互之间_没有_联系，则说明您的方法做了多件事，则应该对其进行拆分。

在某些情况下，输出是由多件事组成的逻辑实体。通过返回结构或对象可以很容易表示：

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

而不是

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

特别是与多个 EXPORTING 参数相比，在该方法中人们可以使用函数式调用，您可以不考虑 `IS SUPPLIED`，并且避免出现人们意外忘记检索重要的 `ERROR_OCCURRED` 信息的情况。

不使用，而是考虑根据有意义的调用模式拆分方法去替代多个可选的输出参数：

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

### 参数类型

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#parameter-types)

#### RETURNING 优于 EXPORTING

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数类型](#parameter-types) > [本节](#prefer-returning-to-exporting)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

而不是毫无必要地加长

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

`RETURNING` 不仅可以使调用更短，还允许使用方法链并防止[相同输入和输出错误](#take-care-if-input-and-output-could-be-the-same)。

#### RETURNING 大表通常没有问题

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数类型](#parameter-types) > [本节](#returning-large-tables-is-usually-okay)

尽管 ABAP 语言文档和性能指南有不一样的说法，但我们很少遇到在 VALUE 参数中传递大表或深度嵌套表_确实_导致性能问题的情况。因此，我们建议正常使用

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

只有在您的个别情况有实际证据（= 不良的性能衡量）时，您才应该使用更繁琐的过程式调用

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

> 本节与 ABAP Programming Guidelines 和代码分析器检查相矛盾，二者都建议应通过引用导出大表，以避免性能下降。但我们始终未能重现任何性能下降和内存不足情况，也未收到有关内核优化（通常可提高 RETURNING 性能）的通知。

#### 单独使用 RETURNING 或 EXPORTING 或 CHANGING，而不要组合使用

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数类型](#parameter-types) > [本节](#use-either-returning-or-exporting-or-changing-but-not-a-combination)

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

而不是混合使用，例如

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

不同种类的输出参数表明该方法做了多件事。这使读者感到困惑，并使调用该方法变得不必要的复杂。

此规则的可接受的例外情况是使用他们的输入构建他们的输出：

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

然而，可以把输入对象化参数使这些内容更加清晰：

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### 在合适时保守地使用 CHANGING

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数类型](#parameter-types) > [本节](#use-changing-sparingly-where-suited)

`CHANGING` 应预留给以下情况：现有局部变量已填充仅在某些位置需要更新：

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

不要强迫调用者仅仅为了提供 `CHANGING` 参数而引入不必要的局部变量。不要使用 `CHANGING` 参数来初始填充先前为空的变量。

#### 拆分方法而非使用布尔输入参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数类型](#parameter-types) > [本节](#split-method-instead-of-boolean-input-parameter)

布尔输入参数通常表示一个方法做_两_件事，而不是一件。

```ABAP
" anti-pattern
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

同样，使用单个（因此未命名的）布尔参数的方法调用往往会混淆参数的含义。

```ABAP
" anti-pattern
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

拆分方法可以简化方法的代码并更好地描述不同的意图

```ABAP
update_without_saving( ).
update_and_save( ).
```

普遍认为，对布尔变量使用 setter 是可以的：

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> 有关详细信息，请参阅[1](http://www.beyondcode.org/articles/booleanVariables.html)[2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/)[3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### 参数名称

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#parameter-names)

#### 考虑调用 RETURNING 参数 RESULT

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数名称](#parameter-names) > [本节](#consider-calling-the-returning-parameter-result)

好的方法名称通常可以让`RETURNING` 参数不需要自己的名称这样的好效果。参数名只需要模仿方法名称或重复一些显而易见的内容。

重复成员名称甚至可能产生冲突，需要添加多余的 `me->` 才能解决。

```ABAP
" anti-pattern
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

在此类情况下，只需把参数命名为 `RESULT`，或者类似于 `RV_RESULT` 的名称（如果您喜欢用匈牙利表示法）。

例如在针对方法链返回 `me` 的方法中，或在创建某些对象但不返回创建的实体而仅返回其键值的方法中，如果参数代表的含义_不_明确，则需要为`RETURNING`参数起个名字。

### 参数初始化

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#parameter-initialization)

#### 清除或覆盖 EXPORTING 引用参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数初始化](#parameter-initialization) > [本节](#clear-or-overwrite-exporting-reference-parameters)

引用参数是指可预先填充的现有内存区域。清除或覆盖它们以提供可靠的数据：

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

> 代码分析器和检查管理器会指出从`EXPORTING` 变量未写入值。使用这些静态检查来避免这个可能相当模糊的错误源。

##### 如果输入和输出可能相同则要当心

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数初始化](#parameter-initialization) > [本节](#take-care-if-input-and-output-could-be-the-same)

通常，在方法体里，类型和数据声明之后第一件事把参数清空是一个好主意。这使该语句易于辨别，并避免了后续语句意外使用原来的值。

但是，某些参数配置可能会使用相同的变量作为输入和输出。在这种情况下，之前的 `CLEAR` 语句会在使用输入值之前将其删除，从而产生错误的结果。

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

考虑用 `RETURNING` 替换 `EXPORTING` 来重新设计此类方法。还可以考虑在单个结果计算语句中覆盖 `EXPORTING` 参数。如果都不适合，只能稍后执行 `CLEAR`。

#### 勿清除 VALUE 参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [参数初始化](#parameter-initialization) > [本节](#dont-clear-value-parameters)

通过 `VALUE` 传递的参数会开辟新的独立内存，这些内存区域定义时为空。不要再清除这些区域：

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

`RETURNING` 参数始终是 `VALUE` 参数，因此您永远不需要清除这些参数：

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### 方法体

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#method-body)

#### 做且仅做一件事，把它做好

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [方法体](#method-body) > [本节](#do-one-thing-do-it-well-do-it-only)

一个方法应该做一件事，且只能做一件事。应该采用最好的方法做这件事。

如果满足以下条件，一个方法可能做一件事：

- [输入参数很少](#aim-for-few-importing-parameters-at-best-less-than-three)
- [不包含布尔参数](#split-method-instead-of-boolean-input-parameter)
- [只有一个输出参数](#return-export-or-change-exactly-one-parameter)
- [很小](#keep-methods-small)
- [将抽象降一级](#descend-one-level-of-abstraction)
- 您无法提取有意义的其他方法
- 您无法将其语句分组为有意义的部分

#### 关注愉快路径或错误处理，但非两者兼顾

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [方法体](#method-body) > [本节](#focus-on-the-happy-path-or-error-handling-but-not-both)

由于[_做且仅做一件事，把它做好_](#do-one-thing-do-it-well-do-it-only)规则的专业化要求，方法应该遵循其建立的愉快路径，或在无法建立愉快路径的情况下采用其他错误处理方式，但也可能出现第三种情况。

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

可以分解为

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

或者，强调验证部分

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

#### 将抽象降一级

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [方法体](#method-body) > [本节](#descend-one-level-of-abstraction)

方法中的语句应处于方法本身抽象级别的下一级。相应地，这些语句都应处于相同的抽象级别。

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

而不是混合使用低级（`trim`、`to_upper`、...）和高级（`publish`、...）概念，例如

```ABAP
" anti-pattern
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

找出正确抽象级别的可靠方法是：让该方法的作者用简短的几个单词来解释该方法的功能，而不需要查看代码。他列的功能项就应该是方法应调用的子方法或应执行的语句。

#### 保持方法精简

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [方法体](#method-body) > [本节](#keep-methods-small)

方法应少于 20 条语句，最好为 3 至 5 条语句。

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

仅通过下面的 `DATA` 声明就足以看出相关的方法不止做一件事：

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

当然，在某些情况下，进一步缩小较大的方法没有任何意义。这是完全可以的，只要该方法始终[专注于一件事](#do-one-thing-do-it-well-do-it-only)：

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

但是，验证冗长的代码是否隐藏了更合适的模式仍然有意义：

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> 将方法切割过小可能会对性能产生不良影响，因为这会增加方法调用的次数。[_顾及性能_一节](#mind-the-performance)提供了有关如何平衡整洁代码和性能的指南。

### 控制流

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [本节](#control-flow)

#### 快速失败

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [控制流](#control-flow) > [本节](#fail-fast)

尽早验证并处理失败情景：

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

后面再进行验证更难以辨别和理解，并且可能已经浪费了很多资源。

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

#### CHECK 对 RETURN

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [控制流](#control-flow) > [本节](#check-vs-return)

如果输入不符合预期，是否应使用 `CHECK` 或 `RETURN` 退出方法，人们对此并未达成共识。

尽管 `CHECK` 显式提供了较短的语法，

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

该语句的名称未能说明条件失败时会发生什么情况，因此长格式可能更易于人们理解：

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD:
```

通过反向验证并采用单向控制流，完全可以避免此类问题

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD:
```

无论如何，请考虑不返回任何内容是否真的合适。方法应该提供有意义的结果，即已填充的返回参数或异常。在许多情况下，不返回任何内容都类似于返回 `null`，应该避免这种情况。

> [ABAP Programming Guidelines 中的_退出过程_一节](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)建议在此实例中使用 `CHECK`。社区讨论表明该语句如此不清晰，许多人无法理解程序的行为。

#### 避免在其他位置使用 CHECK

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [方法](#methods) > [控制流](#control-flow) > [本节](#avoid-check-in-other-positions)

不要在方法的初始化部分以外使用 `CHECK`。该语句在不同位置的行为方式不同，可能会造成不明确、意想不到的影响。

例如，[`LOOP` 中的 `CHECK` 结束当前迭代并继续下一个](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm)；人们原本可能期望它结束方法或退出循环。

> 基于 [ABAP Programming Guidelines 中的_退出过程_一节](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)。请注意，这与[循环中 `CHECK` 的关键字引用](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm)相矛盾。

## 错误处理

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#error-handling)

### 消息

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [本节](#messages)

#### 使消息易于查找

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [消息](#messages) > [本节](#make-messages-easy-to-find)

要使通过事务 SE91 的使用位置搜索更容易找到消息，请使用以下模式：

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

如果不需要变量 `message`，请添加编译指示 `##NEEDED`：

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

避免使用以下模式：

```ABAP
" anti-pattern
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

这是一种反面模式，因为：
- 其中包含不可达代码。
- 其测试的条件永远不可能真正实现对等。

### 返回代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [本节](#return-codes)

#### 异常优于返回代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [返回代码](#return-codes) > [本节](#prefer-exceptions-to-return-codes)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

而不是

```ABAP
" anti-pattern
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

与返回代码相比，异常有许多优点：

- 异常能保持方法签名的干净整洁：以 `RETURNING` 参数形式返回方法结果，同时仍抛出异常。返回代码使用其他参数进行错误处理，会污染您的签名。

- 调用者不必立即做出反应，只需按愉快的路径写下代码。异常处理 `CATCH` 可在方法的末尾执行，也可以完全在外部执行。

- 异常可以通过其属性和方法提供有关错误的详细信息。而返回代码则要求您自行设计另一个解决方案，例如还要返回日志。

- 开发环境通过语法错误提醒调用者处理异常。而在没有任何人注意的情况下，返回代码可能会被不小心忽略。

#### 别让故障溜走

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [返回代码](#return-codes) > [本节](#dont-let-failures-slip-through)

如果确实必须使用返回代码，例如因为调用了不在自己控制范围内的函数和较旧的代码，务必确保别让故障溜走。

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

### 异常

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [本节](#exceptions)

#### 异常针对的是错误，而不是正常情况

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [异常](#exceptions) > [本节](#exceptions-are-for-errors-not-for-regular-cases)

```ABAP
" anti-pattern
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

如果情况是正常、有效的，应该使用常规的结果参数进行处理。

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

异常应该反映错误情况，留到您不希望看到的情况下使用。

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

误用异常会误导读者认为实际上一切都顺利的地方出了问题。异常也比正常代码慢得多，因为它们需要进行构建，往往要收集大量上下文信息。

#### 使用基于类的异常

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [异常](#exceptions) > [本节](#use-class-based-exceptions)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

过时的非基于类的异常与返回代码具有相同的功能，不应再使用。

```ABAP
" anti-pattern
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### 抛出

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [本节](#throwing)

#### 使用各自的超类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#use-own-super-classes)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```

考虑为应用程序的每种异常类型创建抽象超类，而不是直接对基础类进行子类化。允许您对_自己的_所有异常执行 `CATCH`。使您可以向所有异常添加通用功能，例如特殊文本处理。`ABSTRACT` 防止人们意外地直接使用这些非描述性错误。

#### 抛出一种类型的异常

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#throw-one-type-of-exception)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

在绝大多数情况下，抛出多种类型的异常没有任何用处。调用者通常既不感兴趣，也没法区分错误情况，因此经常会以相同的方式处理它们——既然如此，为什么一开始要区分它们呢？

```ABAP
" anti-pattern
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

识别不同错误情况的更好解决方案是使用一种异常类型，但添加允许（但不要求）对个别错误情况做出反应的子类，如[使用子类以便调用者能够区分错误情况](#use-sub-classes-to-enable-callers-to-distinguish-error-situations)中所述。

#### 使用子类以便调用者能够区分错误情况

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#use-sub-classes-to-enable-callers-to-distinguish-error-situations)

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

如果有许多不同的错误情况，则改用错误代码：

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

#### 针对可应对的异常抛出 CX_STATIC_CHECK

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#throw-cx_static_check-for-manageable-exceptions)

如果预期会出现异常并可由接收者合理处理，则抛出继承自 `CX_STATIC_CHECK` 的可控异常：用户输入验证失败，缺少存在后备的资源，等等。

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

这种异常类型_必须_在方法签名中给出，并且_必须_被捕获或转发以避免语法错误。这样，用户便能够清楚看到这种异常类型，确保其不会因意外的异常而感到惊讶，并负责对错误情况做出反应。

> 这与 [ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm) 一致，但与 [Robert C. Martin 所著的 _Clean Code_]（其中建议优先使用不可控异常）相矛盾；[异常](sub-sections/Exceptions.md)说明了其中的原因。

#### 针对通常不可恢复的情况抛出 CX_NO_CHECK

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#throw-cx_no_check-for-usually-unrecoverable-situations)

如果异常严重到使接收端不太可能恢复正常工作，则使用 `CX_NO_CHECK`：无法读取必备资源，无法解决请求的依赖项等。

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` _不能_在方法签名中声明，因此它的出现会使用户感到意外。在无法恢复的情况下，这是可以接受的，因为用户无论如何都无法采取有效的操作。

但是，在某些情况下，用户实际上_可能_希望识别并应对这种故障。例如，如果依赖的管理器无法为请求的接口提供一个实现，则会抛出 `CX_NO_CHECK`，因为常规应用程序代码将无法继续执行。但是，可能某个测试报告试图实例化所有事物，以查看其是否有效，并且会简单地将失败报告为列表中的红色条目——该服务应该能够捕获并忽略异常，而不是被强制转储。

#### 针对可避免的异常考虑 CX_DYNAMIC_CHECK

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#consider-cx_dynamic_check-for-avoidable-exceptions)

`CX_DYNAMIC_CHECK` 的用例很少见，通常我们建议使用其他异常类型。但是，如果调用者对是否可能发生异常完全自主控制，您可能要考虑使用这种异常来代替 `CX_STATIC_CHECK`。

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

例如，使用 `cl_abap_math` 类的 `get_db_length_decs` 方法，它告诉您十进制浮点数的位数和小数位数。如果输入参数未反映十进制浮点数，此方法会引发动态异常 `cx_parameter_invalid_type`。通常，将为完全静态类型的变量调用此方法，以便开发人员知道该异常是否会发生。在这种情况下，动态异常能够让调用者省略不必要的 `CATCH` 子句。

#### 针对完全不可恢复的情况进行转储

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#dump-for-totally-unrecoverable-situations)

如果情况严重到可以完全确定接收者不太可能从中恢复，或者清楚地表明了编程错误，请转储而不是抛出异常：获取内存失败，对必须填充的表读取索引失败等。

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

这种行为将阻止任何类型的用户事后执行任何有用的操作。请仅在确定时使用此功能。

#### RAISE EXCEPTION NEW 优于 RAISE EXCEPTION TYPE

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [抛出](#throwing) > [本节](#prefer-raise-exception-new-to-raise-exception-type)

注：自 NW 7.52 起可用。

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

通常短于毫无必要加长的

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

但是，如果大量添加 `MESSAGE`，可能需要坚持使用 `TYPE` 变式：

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception
  MESSAGE e136(messages).
```

### 捕获

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [本节](#catching)

#### 包裹外来异常而非任其侵入代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [错误处理](#error-handling) > [捕获](#catching) > [本节](#wrap-foreign-exceptions-instead-of-letting-them-invade-your-code)

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

[得墨忒耳律](https://en.wikipedia.org/wiki/Law_of_Demeter)建议将事物解耦，而转发来自其他组件的异常是违反了这一原则的。通过捕获这些异常并将其封装在自己的异常类型中，使自己独立于外部代码。

```ABAP
" anti-pattern
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## 注释

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#comments)

### 用代码表达自己而不是靠注释

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#express-yourself-in-code-not-in-comments)

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

而不是

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

整洁代码_并不是_禁止您为代码写注释，而是鼓励您想出_更好的_替代方法。只有想不出替代方法时才使用注释。

> 从性能的角度来看，这个例子受到了质疑，因为将方法缩减到如此之短会严重降低性能。样本测量表明，在运行速度上，重构代码要比原始的脏代码慢 2.13 倍。整洁代码修复输入 `31-02-2018` 需要 9.6 微秒，而脏代码只需 4.5 微秒。当频繁在高性能应用程序中运行此方法时，可能会对性能造成影响；但对于常规用户输入验证，应该是可以接受的。请参阅[顾及性能](#mind-the-performance)一节以处理整洁代码和性能问题。

### 注释绝非坏名称的借口

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#comments-are-no-excuse-for-bad-names)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

与其解释那些坏名称的真正含义或者您选择坏名称的原因，不如去实际改良名称。

```ABAP
" anti-pattern
" checks whether the table input contains entries
DATA(result) = check_table( input ).
```

### 使用方法而非注释来对代码分段

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#use-methods-instead-of-comments-to-segment-your-code)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

这样不但能够更加清晰地体现代码的意图、结构和依赖关系，同时还能避免在块与块之间因临时变量未清空引起的错误。

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

### 写注释是要解释为什么而非是什么

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#write-comments-to-explain-the-why-not-the-what)

```ABAP
" can't fail, existence of >= 1 row asserted above
DATA(first_line) = table[ 1 ].
```

没人需要用自然语言重复代码

```ABAP
" anti-pattern
" select alert root from database by key
SELECT * FROM d_alert_root WHERE key = key.
```

### 设计应放到设计文档里而不是代码里

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#design-goes-into-the-design-documents-not-the-code)

```ABAP
" anti-pattern
" This class serves a double purpose. First, it does one thing. Then, it does another thing.
" It does so by executing a lot of code that is distributed over the local helper classes.
" To understand what's going on, let us at first ponder the nature of the universe as such.
" Have a look at this and that to get the details.
```

没人会认真读——真的。如果人们需要阅读教科书才能使用代码，这可能说明您的代码存在严重的设计问题，应通过其他方式解决。有些代码_确实_需要解释，而不仅仅是一行注释；在这种情况下，请考虑链接设计文档。

### 用 " 而非 * 加注释

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#comment-with--not-with-)

加引号的注释及其注释语句一同缩进

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

加星号的注释往往造成缩进异常

```ABAP
" anti-pattern
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### 将注释放在与其相关的语句前面

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#put-comments-before-the-statement-they-relate-to)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

整洁性强于

```ABAP
" anti-pattern
output = calculate_result( input ).
" delegate pattern
```

且唐突的程度低于

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### 删除代码而非将其注释掉

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#delete-code-instead-of-commenting-it)

```ABAP
" anti-pattern
* output = calculate_result( input ).
```

当您发现类似内容时，请将其删除。这里显然不需要代码，因为应用程序顺利运行并且所有测试都通过了。以后可根据版本历史记录再生删除的代码。如果需要永久保留某一段代码，请将其复制到文件或 `$TMP` 或 `HOME` 对象中。

### 使用 FIXME、TODO 和 XXX 并添加自己的标识

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#use-fixme-todo-and-xxx-and-add-your-id)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME` 指向内部事件正在形成的过小或过大的错误。
- `TODO` 是您要在不久之后编写代码的地方。
- `XXX` 标记出有效但还可以进一步优化的代码。

输入这类注释时，请添加昵称、姓名缩写或用户，这样可方便共同开发者与您联系并可以在不清楚注释的意图时向您询问问题。

### 勿添加方法签名和注释结尾

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#dont-add-method-signature-and-end-of-comments)

方法签名注释对任何人都没有帮助。

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

几十年前，当在检查代码或处理数十页的打印输出内容时，如果看不到方法签名，这些注释可能会对您大有帮助。但现在，所有 ABAP IDE（SE24、SE80、ADT）都可以轻松显示方法签名，因此这些注释只是干扰而已。

> 在基于表单的编辑器 SE24/SE80 中，按_签名_按钮。在 ABAP 开发工具中，选中方法名称然后按 F2，或将 _ABAP 元素信息_视图添加到您的透视图中。

同样，注释结尾也是多余的。几十年前，当程序和函数以及内部嵌套的 IF 长度达到数百行代码时，这些注释可能很有用。但如今的编码风格发生了变化，方法非常简短，可以轻松看到 `ENDIF` 或 `ENDMETHOD` 属于哪个开头语句：

```ABAP
" anti-pattern
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### 勿复制消息文本作为注释

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#dont-duplicate-message-texts-as-comments)

```ABAP
" anti-pattern
" alert category not filled
MESSAGE e003 INTO dummy.
```

消息独立于您的代码而变化，没有人会记得调整注释，这样注释将很快过时甚至变得有误导性，却没有引起任何人注意。

在现代 IDE 中，您可以轻松查看消息背后的文本，例如在 ABAP 开发工具中，选中消息标识并按 Shift+F2。

如果您希望消息更准确，请考虑将消息提取到其自身的方法。

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### ABAP 文档仅适用于公共 API

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#abap-doc-only-for-public-apis)

编写 ABAP 文档来记录公共 API，这意味着这些 API 可供其他团队或应用程序的开发人员使用。不要为内部内容编写 ABAP 文档。

ABAP 文档与所有注释一样都有相同的弱点，也就是说，它很快会过时，然后会变得有误导性。因此，您应该只在有意义的情况下使用，而不要为一切内容强制编写 ABAP 文档。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 4: Good Comments: Javadocs in Public APIs_ 和 _Chapter 4: Bad Comments: Javadocs in Nonpublic Code_。

### 编译指示优于伪注释

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [注释](#comments) > [本节](#prefer-pragmas-to-pseudo-comments)

优先使用编译指示而不是伪注释来抑制 ATC 识别的无关警告和错误。伪注释大部分已过时，并已替换为编译指示。

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" anti-pattern
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

使用 `ABAP_SLIN_PRAGMAS` 程序或 `SLIN_DESC` 表查找过时伪注释与已替换这些伪注释的编译指示之间的映射。

## 格式化

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#formatting)

下面的建议[为阅读而不是书写进行优化](#optimize-for-reading-not-for-writing)。由于 ABAP 的格式优化器没有涵盖它们，其中的部分建议会产生额外的人工工作，以在名称长度等发生变化时重新格式化语句；如果要避免这种情况，请考虑放弃这些规则，例如[对齐同一对象而非不同对象的赋值](#align-assignments-to-the-same-object-but-not-to-different-ones)。

### 保持一致

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#be-consistent)

以相同的方式格式化项目的所有代码。让所有团队成员使用相同的格式化风格。

如果要编辑外来代码，请遵循该项目的格式化风格，而不要坚持自己的个人风格。

如果要随时间更改格式化规则，请使用[重构最佳实践](#how-to-refactor-legacy-code)随时间更新代码。

### 为阅读而不是书写进行优化

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#optimize-for-reading-not-for-writing)

开发人员花费大量时间_阅读_代码。实际上，一天中_编写_代码所占的比例要小得多。

因此，应针对读取和调试（而非编写）来优化代码格式。

例如，应该优先采用

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

而不是

```ABAP
" anti-pattern
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### 激活前使用格式优化器

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#use-the-pretty-printer-before-activating)

在激活对象前应用格式优化器——SE80、SE24 和 ADT 中的 Shift+F1。

如果修改一个大型未格式化旧代码库，可能需要仅对选定行应用格式优化器，以避免产生大量的变更项和传输依赖项。请考虑在单独的传输请求或注释中整齐打印完整的开发对象。

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 5: Formatting: Team Rules_。

### 使用格式优化器团队设置

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#use-your-pretty-printer-team-settings)

始终使用团队设置。在_菜单_ > _实用程序_ > _设置 ..._ > _ABAP 编辑器_ > _格式优化器_下进行指定。

按照团队的协商设置_缩进_和_转换大写/小写_ > _大写关键字_。

> [大写字母与小写字母](sub-sections/UpperVsLowerCase.md)解释了为什么我们没有为关键字的大小写提供明确的指导。
更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 5: Formatting: Team Rules_。

### 每行只有一条语句

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#no-more-than-one-statement-per-line)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

即使某些情况可能使您误以为这是可读的：

```ABAP
" anti-pattern
DATA do_this TYPE i. do_this = input + 3.
```

### 恪守合理的行长度

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#stick-to-a-reasonable-line-length)

遵守最多 120 个字符的行长度。

如果行距不是很宽，人眼可以更舒适地阅读文字——请在UI 设计师或眼动研究人员建议下做出您的选择。在调试或比较相邻的两行源代码时，如果代码行更窄一些，您会心生感激之情的。

老的终端设备的 80 个甚至 72 个字符的限制太过严格了。虽然通常建议使用 100 个字符（这是一个可行的选择），但对于 ABAP 来说，最好使用 120 个字符，这可能是因为这种语言一般较为冗长。

> 提醒一下，您可以在 ADT 中将打印边距配置为 120 个字符，然后在代码视图中将其显示为垂直线。在_菜单_ > _窗口_ > _首选项_ > _常规_ > _编辑器_ > _文本编辑器_下进行配置。

### 紧缩代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#condense-your-code)

```ABAP
DATA(result) = calculate( items ).
```

而不是添加不必要的空格

```ABAP
" anti-pattern
DATA(result)        =      calculate(    items =   items )   .
```

### 添加单一空行来分隔内容，而不要添加多行

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#add-a-single-blank-line-to-separate-things-but-not-more)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

强调这两条语句做的是不同的事情。但没必要
```ABAP
" anti-pattern
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

添加分隔空行可能表明您的方法没有在[做一件事](#do-one-thing-do-it-well-do-it-only)。

### 勿因分隔空行产生困扰

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#dont-obsess-with-separating-blank-lines)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

没有理由养成用空行将代码分开的坏习惯 

```ABAP
" anti-pattern
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

空行实际上仅在您有跨越多行的语句时才有意义

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

### 对齐同一对象而非不同对象的赋值

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#align-assignments-to-the-same-object-but-not-to-different-ones)

为了强调这些事物在某种程度上是属于一起的

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

或者这样更好

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

而对于那些彼此无关的事物，仍保留参差不齐的状态：

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> 更多信息参阅 [Robert C. Martin 所著的 _Clean Code_] 中的 _Chapter 5: Formatting: Horizontal Alignment_。

### 在行尾关闭括号

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#close-brackets-at-line-end)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### 保持单参数调用于一行

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#keep-single-parameter-calls-on-one-line)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

而不是毫无必要地加长

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### 保持参数在调用后面

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#keep-parameters-behind-the-call)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

如果这造成行很长，可以换行，将参数转到下一行：

```ABAP
DATA(sum) = add_two_numbers(
                   value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                   value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### 如果换行，则在调用下缩进参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#if-you-break-indent-parameters-under-the-call)

```ABAP
DATA(sum) = add_two_numbers(
                   value_1 = 5
                   value_2 = 6 ).
```

在其他地方对齐参数将导致很难发现它们所属的对象：

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

但是，如果要避免因名称长度更改而破坏格式，这就是最佳模式了。

### 将多个参数换行

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#line-break-multiple-parameters)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

是的，这浪费了空间。但是如果不这样，就很难确定一个参数在哪里结束而下一个在哪里开始：

```ABAP
" anti-pattern
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### 对齐参数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#align-parameters)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

边距参差不齐，使得很难看到参数的结束位置以及参数值的开始位置：

```ABAP
" anti-pattern
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

>如果要避免因名称长度更改而破坏格式，这就是另一种最佳模式了。

### 如果调用行过长则将其换行

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#break-the-call-to-a-new-line-if-the-line-gets-too-long)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### 缩进并卡到制表位

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#indent-and-snap-to-tab)

将参数关键字缩进 2 个空格，并将参数缩进 4 个空格：

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

如果没有关键字，则将参数缩进 4 个空格。

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

使用 Tab 键缩进。就算多加了一个空格也没有关系。（如果左侧 `DATA(sum) =` 部分的字符数非偶数，则会发生这种情况。）

### 如同方法调用那样缩进内联声明

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#indent-in-line-declarations-like-method-calls)

按照与方法调用相同的方式，使用 VALUE 或 NEW 缩进内联声明：

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### 勿对齐类型子句

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [格式化](#formatting) > [本节](#dont-align-type-clauses)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

变量及其类型属于同一类，因此从视觉上来说应将其分为一组。将 `TYPE` 子句对齐会使人们的分心，并建议这些变量形成一个垂直组，而它们的类型形成一个垂直组。对齐还会产生不必要的编辑开销，当最长变量名的长度发生变化时，需要调整所有的缩进。

```ABAP
" anti-pattern
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

## 测试

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [本节](#testing)

### 原则

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#principles)

#### 编写可测试的代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [原则](#principles) > [本节](#write-testable-code)

编写的所有代码应该允许您以自动方式测试。

如果需要重构您的代码，那就重构。并在开始添加其他功能之前重构。

如果添加到的旧代码结构过于混乱而无法测试，那么至少要对其进行重构到您新添加部分能够测试。

#### 让他人能够进行模拟

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [原则](#principles) > [本节](#enable-others-to-mock-you)

如果要编写供其他人使用的代码，请让他们能够为自己的代码编写单元测试，例如通过在所有对外交互的位置添加接口，提供有助于促进集成测试的测试替身，或应用依赖倒置使他们能够用测试配置替代生产配置。

#### 可读性规则

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [原则](#principles) > [本节](#readability-rules)

让您的测试代码比生产代码更具可读性。您可以通过有效的测试来处理糟糕的生产代码，但是，可能您甚至还未进行测试，就已经把自己弄懵了。

保持您的测试代码保持简单、傻瓜，这样您一年后仍然能够理解它。

遵守标准和模式，使您的同事能够快速了解代码。

#### 勿制作副本或写测试报告

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [原则](#principles) > [本节](#dont-make-copies-or-write-test-reports)

不要通过制作一个开发对象的 `$TMP` 副本并反复研究来处理开发项。其他人不会注意到这些对象，因此不会知道您的工作状态。您可能会在一开始就浪费大量的时间来制作工作副本，之后您也可能会忘记删除副本，这些副本对于系统和依赖项来说是没用的垃圾。（不相信？立刻看看开发系统并检查一下 `$TMP`。）

另外，不要一开始就编写以特定方式调用某些内容的测试报告，并重复此操作以验证您在运行代码时是否一切正常。这是糟糕的手动测试：手动重复测试报告，目视验证一切是否正常。往前迈一步，在单元测试中自动执行此报告，使用自动断言告诉您代码是否仍然正常运行。这样的好处有，，您不需要进行之后必须编写单元测试的工作。其次，您可以节省进行手动重复所用的大量时间，另外还可以避免感到无聊和疲劳。

#### 测试公共项而非私有内部项

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [原则](#principles) > [本节](#test-publics-not-private-internals)

类的公共项部分，尤其是它们实现的接口，相当稳定并且不太可能发生更改。让您的单元测试仅验证公共项，使其稳定可靠，并最大程度地减少重构类时所需的工作量。相比之下，受保护的和私有的内部项可能会通过重构而快速变化，这样，每次重构都会不必要地中断您的测试。

测试私有方法或受保护方法的迫切需求可能是几个早期设计缺陷的警告信号。问问您自己以下问题：

- 您是否意外地在您的类中埋没了一个概念，这个概念本该公开到其自己的类并使用其专用测试套件？

- 您是否忘记将域逻辑与粘合代码分开？例如，直接在类中实施作为操作、确定或验证插入到 BOPF 的域逻辑，或者由 SAP Gateway 作为 `*_DPC_EXT` 数据提供者生成的域逻辑，可能不是上策。

- 接口是不是太过复杂、请求过多无关的数据或者无法轻松进行模拟？

#### 勿困扰于覆盖范围

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [原则](#principles) > [本节](#dont-obsess-about-coverage)

代码覆盖范围可以帮助您找到忘记测试的代码，而不是满足某些随机的 KPI：

不要仅为了达到覆盖范围而在包含或不含虚拟断言的情况下编写测试。最好保留未经测试的内容，以表明您不能安全地进行重构。您可以拥有小于 100％ 的覆盖范围，并且仍然可以进行完美的测试。在某些情况下，例如在构造函数中使用 IF 插入测试替身时，可能无法达到 100％。好的测试往往会针对不同的分支和条件多次覆盖同一条语句。实际上，这些测试的假想覆盖率大于 100％。

### 测试类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#test-classes)

#### 按用途调用局部测试类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试类](#test-classes) > [本节](#call-local-test-classes-by-their-purpose)

```ABAP
CLASS ltc_unit_tests DEFINITION FOR TESTING ... .
CLASS ltc_integration_tests DEFINITION FOR TESTING ... .
CLASS ltc_unit_tests_with_mocks DEFINITION FOR TESTING ... .
```

良好的名称可以揭示测试的级别以及公有设置部分。

```ABAP
" anti-patterns
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " We know that's the class under test - why repeat it?
CLASS ltc_test DEFINITION FOR TESTING ....                      " Of course it's a test, what else should it be?
```

#### 将测试放在局部类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试类](#test-classes) > [本节](#put-tests-in-local-classes)

将单元测试放入被测类的局部测试包含文件中。这样可以确保人们在重构该类时能够找到这些测试，并允许他们通过按一次按键运行所有相关的测试，如[如何执行测试类](#how-to-execute-test-classes)中所述。

将组件测试、集成测试和系统测试放入包含单独全局类的局部测试中。它们与被测的单个类没有直接关系，因此不应随意将它们放在某个相关类中，而是应放在一个单独的类中。将此全局测试类标记为 `FOR TESTING` 和 `ABSTRACT`，以避免在生产代码中意外地引用该类。将测试放到其他类中存在这样的危险，即人们在重构所涉及的类时忽略并忘记运行这些测试。

因此，使用*测试关系*来记录已测试的对象是有好处的。在下面的例子中，可以在类 `recruting` 或 `candidate` 中或通过快捷键 `Shift-Crtl-F12` (Windows) 或 `Cmd-Shift-F12` (macOS) 执行测试类 `hiring_test`。

```abap
"! @testing recruting
"! @testing candidate
class hiring_test defintion
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### 将帮助方法放在帮助类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试类](#test-classes) > [本节](#put-help-methods-in-help-classes)

将若干测试类使用的帮助方法放在帮助类中。通过继承（关系）或委托（具有关系）使帮助方法可用。

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

#### 如何执行测试类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试类](#test-classes) > [本节](#how-to-execute-test-classes)

在 ABAP 开发工具中，按 Ctrl+Shift+F10 运行某个类中的所有测试。按 Ctrl+Shift+F11 以包括覆盖范围测量。按 Ctrl+Shift+F12 还可运行作为测试关系维护的其他类中的测试。

> 在 macOS 中，使用 `Cmd` 而不是 `Ctrl`。

### 被测代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#code-under-test)

#### 赋予被测代码有意义的名称，或使用缺省名称 CUT

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [被测代码](#code-under-test) > [本节](#name-the-code-under-test-meaningfully-or-default-to-cut)

为被测代码的变量提供一个有意义的名称：

```ABAP
DATA blog_post TYPE REF TO ...
```

不要只使用类名称的所有无价值的命名空间和前缀来重复类名称：

```ABAP
" anti-pattern
DATA clean_fra_blog_post TYPE REF TO ...
```

如果您具有不同的测试设置，且对描述对象的变化状态可能会有所帮助：

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

如果想不到起什么名称有意义，请使用缺省名称 `cut`。该缩写代表“code under test”。

```ABAP
DATA cut TYPE REF TO ...
```

特别是在不整洁且令人困惑的测试中，调用变量 `cut` 可以暂时帮助读者查看实际测试的内容。然而，从长远来看，整理测试才是真正有效的方法。

#### 测试接口而非类

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [被测代码](#code-under-test) > [本节](#test-interfaces-not-classes)

[_测试公共项而非私有内部项_](#test-publics-not-private-internals)的实际结果是，使用_接口_输入您的被测代码

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

而不是_类_

```ABAP
" anti-pattern
DATA code_under_test TYPE REF TO some_class.
```

#### 将被测代码的调用提取到自身的方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [被测代码](#code-under-test) > [本节](#extract-the-call-to-the-code-under-test-to-its-own-method)

如果要测试的方法需要大量参数或准备好的数据，有必要将对它的调用提取到它自己的帮助方法中，该方法预设了不那么重要的参数：

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

直接调用原始方法会让您的测试陷入很多无意义的细枝末节：

```ABAP
" anti-pattern
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### 注入

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#injection)

#### 使用依赖倒置注入测试替身

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#use-dependency-inversion-to-inject-test-doubles)

依赖倒置意味着您将所有依赖项传递给构造函数：

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

不要使用 setter 注入，这样做会以非预期方式使用生产代码：

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

不要使用 FRIENDS 注入，这会在替换生产依赖项之前对其进行初始化，产生意想不到的后果。当您重命名内部项后，它将立即中断。它还会绕过构造函数中的初始化。

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

#### 考虑使用 ABAP 测试替身工具

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#consider-to-use-the-tool-abap-test-double)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

与自定义测试替身相比，更短、更容易理解：

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

#### 利用测试工具

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#exploit-the-test-tools)

一般来说，整洁的编程风格可以让您使用标准 ABAP 单元测试和测试替身完成大部分工作。但是，有些工具可以让您以优雅的方式处理更棘手的情况:

- 使用 `CL_OSQL_REPLACE` 服务测试复杂的 OpenSQL 语句，方法是将这些语句重定向到可在不影响系统其余部分的情况下填充测试数据的测试数据仓。

- 使用 CDS 测试框架来测试您的 CDS 视图。

#### 使用测试缝隙作为临时解决办法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#use-test-seams-as-temporary-workaround)

如果所有其他技术都失败了，或者处于旧代码可能无法正常运行的情况下，应避免使用[测试缝隙](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm)来增加可测试性。

尽管第一眼看上去很舒服，但测试接缝是侵入性的，而且容易与私有依赖项纠缠在一起，从长远来看，它们很难保持活性和稳定性。

因此，我们建议避免仅将测试接缝作为一种临时解决方法以将代码重构为更易于测试的形式。

#### 使用 LOCAL FRIENDS 访问依赖倒置的构造函数

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#use-local-friends-to-access-the-dependency-inverting-constructor)

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

#### 勿滥用 LOCAL FRIENDS 侵入被测代码

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#dont-misuse-local-friends-to-invade-the-tested-code)

访问私有成员和受保护成员以插入模拟数据的单元测试很脆弱：当测试代码的内部结构发生变化时，它们会中断。

```ABAP
" anti-pattern
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### 勿更改生产代码来使代码可测试

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#dont-change-the-productive-code-to-make-the-code-testable)

```ABAP
" anti-pattern
IF me->in_test_mode = abap_true.
```

#### 勿子类化来模拟方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#dont-sub-class-to-mock-methods)

不要在单元测试中通过子类化和覆盖方法来模拟方法。尽管这是可行的，但这样的方法很脆弱，在重构代码时测试很容易中断。而且真正的使用者还可能继承您的类，[如果没有明确设计此功能，这可能会令您措手不及](#final-if-not-designed-for-inheritance)。

```ABAP
" anti-pattern
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

要获取旧被测代码，请[改用测试接缝](#use-test-seams-as-temporary-workaround)。测试接缝同样脆弱，但至少不会改变类的生产行为（如通过删除先前的 `FINAL` 标志或将方法的作用域从 `PRIVATE` 更改为 `PROTECTED` 来启用继承时可能发生的行为），仍不失为一种更为整洁的方式。

在编写新代码时，在设计类时应直接考虑此可测试性问题，并找到其他更好的方法。常见的最佳做法包括[求助于其他测试工具](#exploit-the-test-tools)并将问题方法提取到本身具有接口的单独类中。

> [勿更改生产代码来使代码可测试](#dont-change-the-productive-code-to-make-the-code-testable)的更具体的变化形式。

#### 勿模拟不需要的东西

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#dont-mock-stuff-thats-not-needed)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

尽可能精确地定义给定条件：不要设置测试不需要的数据，也不要模拟永远不会调用的对象。这些内容会分散读者对真实运行情况的注意力。

```ABAP
" anti-pattern
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

在某些情况下，根本不需要模拟某些内容，数据结构和数据容器通常就是这种情况。例如，您的单元测试可以使用 `transient_log` 生产版本正常运行，因为该版本只存储数据而不会产生其他影响。

#### 勿构建测试框架

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [注入](#injection) > [本节](#dont-build-test-frameworks)

与集成测试不同，单元测试涉及数据输入和数据输出，所有测试数据都是根据需要动态定义的。

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

不要开始构建用于区分“*测试案例标识*”的框架来决定要提供的数据。生成的代码将会如此冗长而复杂，以至于您无法长期保留这些测试。

```ABAP
" anti-pattern

test_double->set_test_case( 1 ).

CASE me->test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### 测试方法

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#test-methods)

#### 测试方法名称：反映出设想和预期的情形

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试方法](#test-methods) > [本节](#test-method-names-reflect-whats-given-and-expected)

好的名称可以反映出测试的设想内容和预期结果：

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

不恰当的名称则反映操作内容，重复无意义的事实或含糊不清：

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

由于 ABAP 只允许方法名称使用 30 个字符，如果名称过短而无法表达足够的含义，那么添加注释是合理的。在 ABAP 文档中或在测试方法的第一行添加注释都是不错的选择。

如果有很多名称过长的测试方法，这可能意味着您应该将单个测试类拆分为多个测试类，并在类名称中的给定部分予以区别。

#### 使用 given-when-then

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试方法](#test-methods) > [本节](#use-given-when-then)

按照 given-when-then 范式组织测试代码：首先，初始化给定部分中的内容 ("given")，其次调用实际测试的内容 ("when")，再次验证结果 ("then")。

如果 given 或 then 部分过长，您无法再从视觉上区分这三个部分，请提取子方法。空行或注释作为分隔符乍一看可能还不错，但并不能真正减少视觉混乱。尽管如此，空行或注释对于读者和测试新手区分这三个部分还是有所帮助的。

#### "When" 恰为一个调用

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试方法](#test-methods) > [本节](#when-is-exactly-one-call)

确保测试方法的 "when" 部分仅包含对被测类的一次调用：

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

调用多个对象表明该方法没有明确的焦点，测试内容过多。这使得测试失败时更难以找到原因：是第一次、第二次还是第三次调用导致了失败？这也使读者感到困惑，因为他不确定确切的被测功能是什么。

#### 除非真正需要否则勿添加 TEARDOWN

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试方法](#test-methods) > [本节](#dont-add-a-teardown-unless-you-really-need-it)

通常只需要使用 `teardown` 方法来清除数据库条目或集成测试中的其他外部资源。

重置测试类的成员（尤其是 `cut` 和所用的测试替身）是多余的操作；在启动下一个测试方法之前，这些成员就会由 `setup` 方法覆盖。

### 测试数据

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#test-data)

#### 使其易于辨明含义

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试数据](#test-data) > [本节](#make-it-easy-to-spot-meaning)

在单元测试中，您希望能够快速判断出哪些数据和替身是需要关注的内容，哪些内容的作用只是为了防止代码崩溃。通过为没有含义的内容起一个显眼的名称和值来支持此功能，例如：

```ABAP
DATA(alert_id) = '42'.                             " well-known meaningless numbers
DATA(detection_object_type) = '?=/"&'.             " 'keyboard accidents'
CONSTANTS some_random_number TYPE i VALUE 782346.  " revealing variable names
```

不要欺骗人们相信某些内容可以与真实的对象或真实的定制联系起来（如果没有联系）：

```ABAP
" anti-pattern
DATA(alert_id) = '00000001223678871'.        " this alert really exists
DATA(detection_object_type) = 'FRA_SCLAIM'.  " this detection object type, too
CONSTANTS memory_limit TYPE i VALUE 4096.    " this number looks carefully chosen
```

#### 使其易于辨明差异

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试数据](#test-data) > [本节](#make-it-easy-to-spot-differences)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

不要强迫读者去比较长且无意义的字符串来发现细微的差别。

#### 使用常量描述测试数据的用途和重要性

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [测试数据](#test-data) > [本节](#use-constants-to-describe-purpose-and-importance-of-test-data)

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

### 断言

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [本节](#assertions)

#### 少而精的断言

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#few-focused-assertions)

仅使用少量断言，准确地断言测试方法的内容。

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

断言过多表明该方法没有明确的重点。这会在很多地方将生产和测试代码耦合在一起：更改功能将需要重写大量的测试，尽管它们实际上并未涉及更改后的功能。各种各样的断言使读者感到困惑，难以辨别其中最重要的那个断言。

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

#### 使用恰当的断言类型

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#use-the-right-assert-type)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

断言的作用往往不止表面看到的那些，例如 `assert_equals` 包括类型匹配，如果值不同，还能提供准确的描述。使用错误的、过于常见的断言将迫使您立即进入调试器，而不是让您直接从错误消息中看到问题所在。

```ABAP
" anti-pattern
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### 断言内容而非数量

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#assert-content-not-quantity)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

如果可以表达期望的实际内容，就不要编写幻数数量断言。尽管仍然可以达到预期，但数字可能会有所不同。相反，尽管内容完全出乎意料，但数字可能会匹配。

```ABAP
" anti-pattern
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### 断言质量而非内容

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#assert-quality-not-content)

如果您对结果的元质量感兴趣，但对实际内容本身不感兴趣，请使用合适的断言来表达：

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

断言精确的内容会掩盖您实际想要测试的内容。它还很脆弱，因为重构可能会产生一个不同但完全可以接受的结果，尽管它会中断所有过于精确的单元测试。

```ABAP
" anti-pattern
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### 使用 FAIL 检查是否出现预期异常

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#use-fail-to-check-for-expected-exceptions)

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

#### 转发意外异常而非捕获就失败

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#forward-unexpected-exceptions-instead-of-catching-and-failing)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

与以下代码相比，您的测试代码始终专注于愉快路径，因此更易于阅读和理解：

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

#### 编写自定义断言以缩短代码和避免重复

> [ABAP 整洁之道](#clean-abap) > [目录](#content) > [测试](#testing) > [断言](#assertions) > [本节](#write-custom-asserts-to-shorten-code-and-avoid-duplication)

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

而不是一遍又一遍地复制粘贴。
