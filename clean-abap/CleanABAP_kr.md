> Translated from [English original on 14.10.2021](https://github.com/SAP/styleguides/tree/72ecf7fd7d41151d5bbca29020d4ec9de953db8c).
> Latest version [in English](CleanABAP.md).
> 
# Clean ABAP

> [**한국어**](CleanABAP_kr.md)
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
> [Español](CleanABAP_es.md)
> &nbsp;·&nbsp;
> [한국어](CleanABAP_kr.md)
> &nbsp;·&nbsp;
> [Русский](CleanABAP_ru.md)

이 가이드는 [ABAP](https://en.wikipedia.org/wiki/ABAP) 용
[로버트 마틴의 _Clean Code_]라는 책과 관련이 있습니다.

[Cheat Sheet](cheat-sheet/CheatSheet.md) 라는 파일은 프린트용 버전입니다.

[로버트 마틴의 _Clean Code_]: http://www.yes24.com/Product/Goods/11681152 "한국링크"

## 내용

- [방법](#방법)
    - [Clean Code를 시작하는 방법](#Clean-Code를-시작하는-방법)
    - [레거시 코드를 리팩토링하는 방법](#레거시-코드를-리팩토링하는-방법)
    - [자동으로 점검하는 방법](#자동으로-점검하는-방법)
    - [다른 가이드와 연결하는 방법](#다른-가이드와-연결하는-방법)
    - [동의하지 않는 방법](#동의하지-않는-방법)
- [네이밍](#네이밍)
    - [설명이 포함된 이름을 사용하라](#설명이-포함된-이름을-사용하라)
    - [솔루션 도메인이나 문제와 관련된 도메인 용어를 사용하라](#솔루션-도메인이나-문제와-관련된-도메인-용어를-사용하라)
    - [복수형을 사용하라](#복수형을-사용하라)
    - [발음하기 쉬운 이름을 사용하라](#발음하기-쉬운-이름을-사용하라)
    - [약어를 피하라](#약어를-피하라)
    - [모든 곳에서 동일한 약어를 사용하라 ](#모든-곳에서-동일한-약어를-사용하라)
    - [클래스에는 명사를 사용하고 메서드에는 동사를 사용하라](#클래스에는-명사를-사용하고-메서드에는-동사를-사용하라)
    - [불필요한 단어를 피하라](#불필요한-단어를-피하라)
    - [개념당 하나의 단어를 선택하라](#개념당-하나의-단어를-선택하라)
    - [의미 있는 경우에만 패턴 이름을 사용하라](#의미-있는-경우에만-패턴-이름을-사용하라)
    - [헝가리안 표기법과 접두사 같은 불필요한 인코딩을 피하라](#헝가리안-표기법과-접두사-같은-불필요한-인코딩을-피하라)
- [언어](#언어)
    - [레가시를 신경 써라](#레가시를-신경-써라)
    - [수행속도를 신경 써라](#수행속도를-신경-써라)
    - [절차지향적 프로그래밍보다 객체지향적 프로그래밍을 선호하라](#절차지향적-프로그래밍보다-객체지향적-프로그래밍을-선호하라)
    - [절차지향적 언어 구성보다 기능적인 구성을 선호하라](#절차지향적-언어-구성보다-기능적인-구성을-선호하라)
    - [쓸모 없는 언어 요소를 피하라](#쓸모-없는-언어-요소를-피하라)
    - [디자인 패턴을 현명하게 사용하라](#디자인-패턴을-현명하게-사용하라)
- [상수](#상수)
    - [매직넘버 대신에 상수를 사용하라](#매직넘버-대신에-상수를-사용하라)
    - [상수 인터페이스보다 열거체를 선호하라](#상수-인터페이스보다-열거체를-선호하라)
    - [열거체를 사용하지 않는다면 상수들을 그룹화하라](#열거체를-사용하지-않는다면-상수들을-그룹화하라)
- [변수](#변수)
    - [변수는 사용하는 위치에 최대한 가까이 선언하라](#변수는-사용하는-위치에-최대한-가까이-선언하라)
    - [분기점에서 인라인을 선언하지 마라](#분기점에서-인라인을-선언하지-마라)
    - [선행 선언을 서로 묶지 마라](#선행-선언을-서로-묶지-마라)
    - [FIELD-SYMBOL보다 REF TO를 사용하라](#FIELD-SYMBOL보다-REF-TO를-사용하라)
- [테이블](#테이블)
    - [올바른 테이블 유형을 사용하라](#올바른-테이블-유형을-사용하라)
    - [기본 키 사용을 피하라](#기본-키-사용을-피하라)
    - [APPEND TO 보다 INSERT INTO TABLE을 사용하라 ](#APPEND-TO-보다-INSERT-INTO-TABLE을-사용하라)
    - [READ TABLE이나 LOOP AT보다 LINE EXISTS 구문을 사용하라](#READ-TABLE이나-LOOP-AT보다-LINE-EXISTS-구문을-사용하라)
    - [LOOP AT보다 READ TABLE 구문을 사용하라](#LOOP-AT보다-READ-TABLE-구문을-사용하라)
    - [중첩된 IF문보다 LOOP AT 구문을 사용하라](#중첩된-IF문보다-LOOP-AT-구문을-사용하라)
    - [불필요한 테이블 읽는 것을 피하라](#불필요한-테이블-읽는-것을-피하라)
- [문자열](#문자열)
    - [리터럴을 정의하기 위해 억음부호를 사용하라](#리터럴을-정의하기-위해-억음부호를-사용하라)
    - [텍스트를 이을 때는 파이프 문자를 사용하라](#텍스트를-이을-때는-파이프-문자를-사용하라)
- [Booleans](#booleans)
    - [Boolean을 현명하게 사용하라](#Boolean을-현명하게-사용하라)
    - [Boolean에서는 ABAP_BOOL을 사용하라](#Boolean에서는-ABAP_BOOL을-사용하라)
    - [서로를 비교할 때는 ABAP_TRUE와 ABAP_FALSE를 사용하라](#서로를-비교할-때는-ABAP_TRUE와-ABAP_FALSE를-사용하라)
    - [Boolean 변수를 설정할 때는 XSDBOOL을 사용하라](#Boolean-변수를-설정할-때는-XSDBOOL을-사용하라)
- [컨디션](#컨디션)
    - [컨디션을 긍정문으로 작성하라](#컨디션을-긍정문으로-작성하라)
    - [NOT IS보다 IS NOT을 사용하라](#NOT-IS보다-IS-NOT을-사용하라)
    - [복잡한 컨디션들을 쪼개는 것을 고려하라](#복잡한-컨디션들을-쪼개는-것을-고려하라)
    - [복잡한 컨디션들을 메서드화하라](#복잡한-컨디션들을-메서드화하라)
- [조건문](#조건문)
    - [IF분기에서 빈 곳을 만들지 마라](#IF분기에서-빈-곳을-만들지-마라)
    - [여러 대안이 있다면 ELSE IF 대신에 CASE를 사용하라](#여러-대안이-있다면-ELSE-IF-대신에-CASE를-사용하라)
    - [분기를 최대한 적게 하라](#분기를-최대한-적게-하라)
- [정규식](#정규식)
    - [정규식 대신에 간단한 메서드를 사용하라](#정규식-대신에-간단한-메서드를-사용하라)
    - [정규식 대신에 기본적인 체크를 하라](#정규식-대신에-기본적인-체크를-하라)
    - [복잡한 정규식들을 조합하는 것을 고려하라](#복잡한-정규식들을-조합하는-것을-고려하라)
- [클래스](#클래스)
    - [객체 지향 클래스](#객체-지향-클래스)
        - [정적인 클래스보다 오브젝트를 사용하라](#정적인-클래스보다-오브젝트를-사용하라)
        - [상속 대신에 구성을 사용하라](#상속-대신에-구성을-사용하라)
        - [동일한 클래스에서 상태 저장과 비저장을 혼재하지 마라](#동일한-클래스에서-상태-저장과-비저장을-혼재하지-마라)
    - [범위](#범위)
        - [기본적으로 전역 클래스를 사용하고 적절한 경우에만 로컬 클래스를 사용하라](#기본적으로-전역-클래스를-사용하고-적절한-경우에만-로컬-클래스를-사용하라)
        - [상속을 고려하지 않았다면 FINAL을 사용하라](#상속을-고려하지-않았다면-FINAL을-사용하라)
        - [기본적으로 Private를 사용하고 필요한 경우에만 Protected를 사용하라](#기본적으로-Private를-사용하고-필요한-경우에만-Protected를-사용하라)
        - [Getter 메서드 대신에 PUBLIC과 READ-ONLY를 사용하라](#Getter-메서드-대신에-PUBLIC과-READ-ONLY를-사용하라)
        - [READ-ONLY를 남용하지 마라](#READ-ONLY를-남용하지-마라)
    - [생성자](#생성자)
        - [CREATE OBJECT 대신에 NEW를 사용하라](#CREATE-OBJECT-대신에-NEW를-사용하라)
        - [전역 클래스가 CREATE private라면 생성자를 public으로 놔두어라](#전역-클래스가-CREATE-private라면-생성자를-public으로-놔두어라)
        - [선택적 파라미터보다 여러 정적인 생성 메서드를 사용하라](#선택적-파라미터보다-여러-정적인-생성-메서드를-사용하라)
        - [여러 생성 메서드를 만들 때는 서술적인 이름을 사용하라](#여러-생성-메서드를-만들-때는-서술적인-이름을-사용하라)
        - [여러 인스턴스가 있는 곳에서만 싱글톤을 만드는 것은 말이 되지 않는다](#여러-인스턴스가-있는-곳에서만-싱글톤을-만드는-것은-말이-되지-않는다)
- [메서드](#메서드)
    - [호출](#호출)
        - [절차적인 콜보다 펑선콜을 사용하라](#절차적인-콜보다-펑선콜을-사용하라)
        - [RECEIVING을 생략하라](#RECEIVING을-생략하라)
        - [불필요한 EXPORTING 구문을 생략하라](#불필요한-EXPORTING-구문을-생략하라)
        - [단일 파라미터 콜에서는 파라미터명을 생략하라](#단일-파라미터-콜에서는-파라미터명을-생략하라)
        - [인스턴스 메서드를 호출할 때는 자기 참조를 생략하라](#인스턴스-메서드를-호출할-때는-자기-참조를-생략하라)
    - [객체 지향 메서드](#객체-지향-메서드)
        - [정적 메서드보다 인스턴스를 사용하라](#정적-메서드보다-인스턴스를-사용하라)
        - [공용 인스턴스 메서드는 항상 인터페이스의 일부여야 한다](#공용-인스턴스-메서드는-항상-인터페이스의-일부여야-한다)
    - [파라미터 개수](#파라미터-개수)
        - [IMPORTING 파라미터를 3개 이하로 조정하라](#IMPORTING-파라미터를-3개-이하로-조정하라)
        - [옵션 파라미터를 추가하는 대신 메서드를 분할하라](#옵션-파라미터를-추가하는-대신-메서드를-분할하라)
        - [우선적 파라미터를 남용하지 마라](#우선적-파라미터를-남용하지-마라)
        - [RETURN이나 EXPORT나 CHANGE 중에 하나만 사용하라](#RETURN이나-EXPORT나-CHANGE-중에-하나만-사용하라)
    - [파라미터 유형](#파라미터-유형)
        - [EXPORTING보다 RETURNING을 사용하라](#EXPORTING보다-RETURNING을-사용하라)
        - [큰 테이블을 다룰 때는 RETURNING을 사용하라](#큰-테이블을-다룰-때는-RETURNING을-사용하라)
        - [RETURNING이나 EXPORTING이나 CHANGING에 하나를 사용하고 혼합하여 사용하지 마라](#RETURNING이나-EXPORTING이나-CHANGING에-하나를-사용하고-혼합하여-사용하지-마라)
        - [CHANGING을 분별하여 사용하라](#CHANGING을-분별하여-사용하라)
        - [Boolean 파라미터를 사용하는대신에 메서드를 분할하라](#Boolean-파라미터를-사용하는-대신에-메서드를-분할하라)
    - [파라미터명](#파라미터명)
        - [RETURNING 파라미터가 RESULT를 호출하도록 하라](#RETURNING-파라미터가-RESULT를-호출하도록-하라)
    - [파라미터 초기화](#파라미터-초기화)
        - [EXPORTING 참조 파라미터를 초기화하거나 덮어쓰기하라](#EXPORTING-참조-파라미터를-초기화하거나-덮어쓰기하라)
            - [입력과 출력이 동시에 이루어지지 않는지 신경 써라](#입력과-출력이-동시에-이루어지지-않는지-신경-써라)
        - [Value 파라미터를 초기화하지 마라](#Value-파라미터를-초기화하지-마라)
    - [메서드 바디](#메서드-바디)
        - [하나의-메서드는-한-가지만을-수행하라](#하나의-메서드는-한-가지만을-수행하라)
        - [HAPPY-PATH 혹은 에러 처리 둘 중 하나에 집중하라](#HAPPY-PATH-혹은-에러-처리-둘-중-하나에-집중하라)
        - [한 단계의 추상화 레벨만을 가져라](#한-단계의-추상화-레벨만을-가져라)
        - [메서드를 작게 유지하라](#메서드를-작게-유지하라)
    - [제어 흐름](#제어-흐름)
        - [검증을 빠르게 하라](#검증을-빠르게-하라)
        - [CHECK vs RETURN](#check-vs-return)
        - [일부 위치에서는 CHECK 사용을 지양하라](#일부-위치에서는-CHECK-사용을-지양하라)
- [에러 처리](#에러-처리)
    - [메시지](#메시지)
        - [메시지를 찾기 쉽게 만들어라](#메시지를-찾기-쉽게-만들어라)
    - [리턴 코드](#리턴-코드)
        - [리턴 코드보다 예외 구문을 사용하라](#리턴-코드보다-예외-구문을-사용하라)
        - [오류를 놓치지 않도록 만들어라](#오류를-놓치지-않도록-만들어라)
    - [예외 처리](#예외-처리)
        - [일반적인 케이스가 아닐 때의 에러는 예외 구문을 사용하라](#일반적인-케이스가-아닐-때의-에러는-예외-구문을-사용하라)
        - [클래스에 기반한 예외 구문을 사용하라](#클래스에-기반한-예외-구문을-사용하라)
    - [예외 처리 구문](#예외-처리-구문)
        - [슈퍼 클래스를 사용하라](#슈퍼-클래스를-사용하라)
        - [하나의 유형만으로 예외 처리하라](#하나의-유형만으로-예외-처리하라)
        - [호출자가 오류 상황을 구별할 수 있도록 하위 클래스를 사용하라](#호출자가-오류-상황을-구별할-수-있도록-하위-클래스를-사용하라)
        - [관리 가능한 예외 처리 구문에 대해 CX_STATIC_CHECK 구문을 사용하라](#관리-가능한-예외-처리-구문에-대해-CX_STATIC_CHECK-구문을-사용하라)
        - [복구할 수 없는 상활일 때에는 CX_NO_CHECK 구문을 사용하라](#복구할-수-없는-상활일-때에는-CX_NO_CHECK-구문을-사용하라)
        - [회피할 수 있는 상황에서는 CX_DYNAMIC_CHECK 구문을 사용하라](#회피할-수-있는-상황에서는-CX_DYNAMIC_CHECK-구문을-사용하라)
        - [완전히 복구가 불가능한 상황에서는 DUMP 처리하라](#완전히-복구가-불가능한-상황에서는-DUMP-처리하라)
        - [RAISE EXCEPTION TYPE보다 RAISE EXCEPTION NEW를 사용하라](#RAISE-EXCEPTION-TYPE보다-RAISE-EXCEPTION-NEW를-사용하라)
    - [Catch 구문](#catch-구문)
        - [외부에서 당신의 코드를 침범하지 못하도록 예외 처리하라](#외부에서-당신의-코드를-침범하지-못하도록-예외-처리하라)
- [주석](#주석)
    - [당신의 생각을 주석이 아닌 코드에 표현하라](#당신의-생각을-주석이-아닌-코드에-표현하라)
    - [주석으로 핑계를 대지 말고 코드를 변경하라](#주석으로-핑계를-대지-말고-코드를-변경하라)
    - [코드를 나눠서 주석하는 대신에 메서드를 사용하라](#코드를-나눠서-주석하는-대신에-메서드를-사용하라)
    - [코드에 대한 이유를 주석으로 작성하라](#코드에-대한-이유를-주석으로-작성하라)
    - [설계에 관련된 내용을 주석으로 작성하지 마라](#설계에-관련된-내용을-주석으로-작성하지-마라)
    - [별이 아닌 큰 따옴표를 사용하라](#별이-아닌-큰-따옴표를-사용하라)
    - [코드 바로 위에 주석을 작성하라](#코드-바로-위에-주석을-작성하라)
    - [주석 처리한 코드는 삭제하라](#주석-처리한-코드는-삭제하라)
    - [FIXME, TODO, XXX 그리고 당신의 아이디를 사용하라](#FIXME-TODO-XXX-그리고-당신의-아이디를-사용하라)
    - [메서드 서명이나 끝에 주석을 달지 마라](#메서드-서명이나-끝에-주석을-달지-마라)
    - [메시지 텍스트를 주석으로 재사용하지 마라](#메시지-텍스트를-주석으로-재사용하지-마라)
    - [공개 API를 문서화하라](#공개-API를-문서화하라)
    - [슈도 주석보다 프라그마를 사용하라](#슈도-주석보다-프라그마를-사용하라)
- [서식](#서식)
    - [동일한 양식을 유지하라](#동일한-양식을-유지하라)
    - [쓰기가 아니라 읽기를 위한 최적화를 하라](#쓰기가-아니라-읽기를-위한-최적화를-하라)
    - [Activate 버튼을 누르기 전에 Pretty Printer를 먼저 하라](#Activate-버튼을-누르기-전에-Pretty-Printer를-먼저-하라)
    - [Pretty Printer 설정은 팀에서 정한대로 따라가라](#Pretty-Printer-설정은-팀에서-정한대로-따라가라)
    - [한 줄에는 하나의 구문만 존재하게 하라](#한-줄에는-하나의-구문만-존재하게-하라)
    - [합리적인 라인별 코드 길이를 준수하라](#합리적인-라인별-코드-길이를-준수하라)
    - [불필요한 공백을 줄여라](#불필요한-공백을-줄여라)
    - [역할을 구분하기 위해서는 한 줄만 추가하라](#역할을-구분하기-위해서는-한-줄만-추가하라)
    - [굳이 빈 줄을 추가하지 마라](#굳이-빈-줄을-추가하지-마라)
    - [동일한 오브젝트에 한해서는 라인을 정렬하라](#동일한-오브젝트에-한해서는-라인을-정렬하라)
    - [괄호는 라인의 끝에 위치하게 하라](#괄호는-라인의-끝에-위치하게-하라)
    - [단일 파라미터 콜은 한 줄에 작성하라](#단일-파라미터-콜은-한-줄에-작성하라)
    - [호출문 뒤에 파라미터를 위치하게 하라](#호출문-뒤에-파라미터를-위치하게-하라)
    - [호출문 아래 파라미터들을 들여쓰기하라](#호출문-아래-파라미터들을-들여쓰기하라)
    - [파라미터가 여러 개일 때는 라인을 분리하라](#파라미터가-여러-개일-때는-라인을-분리하라)
    - [파라미터들을 정렬하라](#파라미터들을-정렬하라)
    - [코드가 너무 긴 경우에는 새로운 라인으로 호출문을 분리하라](#코드가-너무-긴-경우에는-새로운-라인으로-호출문을-분리하라)
    - [Tab 버튼을 활용해 들여쓰기하라](#Tab-버튼을-활용해-들여쓰기하라)
    - [메서드 호출인 것처럼 들여쓰기하라](#메서드-호출인-것처럼-들여쓰기하라)
    - [Type절은 정렬하지 마라](#Type절은-정렬하지-마라)
- [테스트](#테스트)
    - [테스트 원칙](#테스트-원칙)
        - [테스트가 가능하도록 코딩하라](#테스트가-가능하도록-코딩하라)
        - [다른 사람이 자신의 코드를 테스트 가능하도록 코딩하라](#다른-사람이-자신의-코드를-테스트-가능하도록-코딩하라)
        - [가독성 확보를 위한 규칙들을 정하라](#가독성-확보를-위한-규칙들을-정하라)
        - [테스트 보고서를 복사하거나 작성하지 마라](#테스트-보고서를-복사하거나-작성하지-마라)
        - [Private가 아닌 Public으로 테스트하라](#Private가-아닌-Public으로-테스트하라)
        - [코드 커버리지에 집착하지 마라](#코드-커버리지에-집착하지-마라)
    - [테스트 클래스](#테스트-클래스)
        - [목적에 따라 로컬 테스트 클래스 호출하라](#목적에-따라-로컬-테스트-클래스-호출하라)
        - [로컬 클래스에 단위 테스트를 포함시켜라](#로컬-클래스에-단위-테스트를-포함시켜라)
        - [도움말 클래스에 도움말 메서드를 포함시켜라](#도움말-클래스에-도움말-메서드를-포함시켜라)
        - [테스트 클래스를 실행하는 방법](#테스트-클래스를-실행하는-방법)
    - [테스트 중인 코드](#테스트-중인-코드)
        - [테스트 중인 코드를 의미 있게 네이밍하거나 CUT 코드를 사용하라](#테스트-중인-코드를-의미-있게-네이밍하거나-CUT-코드를-사용하라)
        - [클래스 구현이 아닌 인터페이스를 테스트하라](#클래스-구현이-아닌-인터페이스를-테스트하라)
        - [테스트 중인 코드에 대한 호출을 자체 메서드로 추출하라](#테스트-중인-코드에-대한-호출을-자체-메서드로-추출하라)
    - [인젝션](#인젝션)
        - [테스트 더블을 인젝션할 때 종속성 반전을 사용하라](#테스트-더블을-인젝션할-때-종속성-반전을-사용하라)
        - [ABAP 테스트 더블 툴 사용을 고려하라](#ABAP-테스트-더블-툴-사용을-고려하라)
        - [테스트 도구를 활용하라](#테스트-도구를-활용하라)
        - [테스트 이음새는 임시 해결책으로만 활용하라](#테스트-이음새는-임시-해결책으로만-활용하라)
        - [종속성 반전 생성자에 접근할 때는 LOCAL FRIENDS를 사용하라](#종속성-반전-생성자에-접근할-때는-LOCAL-FRIENDS를-사용하라)
        - [테스트된 코드에 영향을 주면서까지 LOAL FRIENDS를 오용하지 마라](#테스트된-코드에-영향을-주면서까지-LOCAL-FRIENDS를-오용하지-마라)
        - [코드를 테스트 가능하도록 만들기 위해 기존의 코드를 변경하지 마라](#코드를-테스트-가능하도록-만들기-위해-기존의-코드를-변경하지-마라)
        - [메서드를 mocking하기 위해 sub-class를 만들지 마라](#메서드를-mocking하기-위해-sub-class를-만들지-마라)
        - [필요하지 않은 것까지 mocking하지 마라](#필요하지-않은-것까지-mocking하지-마라)
        - [테스트 프레임워크를 구축하지 마라](#테스트-프레임워크를-구축하지-마라)
    - [테스트 메서드](#테스트-메서드)
        - [상황이 잘 묘사되도록 테스트 메서드를 네이밍하라](#상황이-잘-묘사되도록-테스트-메서드를-네이밍하라)
        - [given-when-then을 사용하라](#given--when--then을-사용하라)
        - [when 구문을 사용할 때는 정확히 하나만을 호출하라](#when-구문을-사용할-때는-정확히-하나만을-호출하라)
        - [정말 필요한 경우가 아니라면 TEARDOWN을 추가하지 마라](#정말-필요한-경우가-아니라면-TEARDOWN을-추가하지-마라)
    - [테스트 데이터](#테스트-데이터)
        - [의미를 쉽게 이해할 수 있도록 테스트 데이터를 만들어라](#의미를-쉽게-이해할-수-있도록-테스트-데이터를-만들어라)
        - [차이점을 쉽게 이해할 수 있도록 테스트 데이터를 만들어라](#차이점을-쉽게-이해할-수-있도록-테스트-데이터를-만들어라)
        - [테스트 데이터의 목적과 중요성을 설명할 때는 상수를 사용하라](#테스트-데이터의-목적과-중요성을-설명할-때는-상수를-사용하라)
    - [어써션](#어써션)
        - [어써션을 최소화하라](#어써션을-최소화하라)
        - [올바른 어써션 유형을 사용하라](#올바른-어써션-유형을-사용하라)
        - [수량이 아닌 내용을 어쎠선하라](#수량이-아닌-내용을-어쎠선하라)
        - [내용이 아닌 품질을 어써션하라](#내용이-아닌-품질을-어써션하라)
        - [예외를 처리할 때는 FAIL 구문을 사용하라](#예외를-처리할-때는-FAIL-구문을-사용하라)
        - [Try-Catch 구문을 사용하는 대신에 when-then 구문을 사용하라](#Try-Catch-구문을-사용하는-대신에-when-then-구문을-사용하라)
        - [코드를 줄이고 중복을 피하기 위해 사용자 지정 어써션을 작성하라](#코드를-줄이고-중복을-피하기-위해-사용자-지정-어써션을-작성하라)

## 방법

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#방법)

### Clean Code를 시작하는 방법

> [Clean ABAP](#clean-abap) > [내용](#내용) > [방법](#방법) > [이번 장](#Clean-Code를-시작하는-방법)

만약 당신이 'Clean Code' 초심자라면, 당신은 먼저 [로버트 마틴의 _Clean Code_]라는 책을 읽어야합니다. 
[Clean Code 개발자 포털](https://clean-code-developer.com/) 사이트는 
step-by-step으로 Clean Code를 시작하는 데 도움을 줍니다.

우리는 당신이 [Booleans](#booleans), [컨디션](#컨디션), [조건문](#조건문)과 같이 
쉽게 이해할 수 있고 폭넓게 적용할 수 있는 것들부터 시작하기를 권합니다.

당신은 아마 [메서드](#메서드) 섹션(특히 [한가지만 잘해봐!](#하나의-메서드는-한-가지만을-수행하라), [메서드를 작게 유지하라](#메서드를-작게-유지하라))에서 
가장 많은 도움을 받을 수 있을 거라 생각되는데, 이는 코드의 전반적인 구조를 엄청나게 향상시키기 때문입니다.

몇몇 Clean Code 주제들은 그들이 하고 있는 일에 반하기 때문에
팀에서 어려운 과정들이 있을 수 있습니다;
이러한 주제들은 완벽히 '건강한' 것이지만, 어쩌면 당연하게도 성장통을 겪을 것입니다. 

어느 정도 숙련이 되면 논란의 여지가 있는 주제들을 계속 다루어야 하는데,
특히 [주석](#주석), [네이밍](#네이밍), [서식](#서식)과 같은 내용은 거의 종교적 분쟁 수준입니다.
따라서 Clean Code의 긍정적인 효과를 직접 경험한 팀에서만 다루어져야 합니다.

### 레거시 코드를 리팩토링하는 방법

> [Clean ABAP](#clean-abap) > [내용](#내용) > [방법](#방법) > [이번 장](#레거시-코드를-리팩토링하는-방법)

당신이 변경할 수 없거나 변경을 원하지 않는 수많은 코드들이 난무하는 레가시 프로젝트에서 일하고 있는 경우,
[논리 자료형](#booleans), [컨디션](#컨디션), [조건문](#조건문) 그리고 [메서드](#메서드)는 당신에게 가장 도움이 됩니다.
왜냐하면, 그것들은 충돌 없이도 새로운 Clean Code들로 적용이 가능하기 때문입니다.

[네이밍](#네이밍)은 특히나 레가시 프로젝트에서 매우 복잡할 수가 있는데,
새로운 코드와 기존의 코드 사이에 연결이 끊어지거나 구멍이 생길 수 있기 때문입니다.
그러한 과정에서 [헝가리안 표기법과 접두사 같은 불필요한 인코딩을 피하라](#헝가리안-표기법과-접두사-같은-불필요한-인코딩을-피하라)
와 같은 주제들은 어느 정도 무시하는 것이 나을 정도입니다.

리팩토링을 수행할 때 동일한 개발 객체 내에서 다른 개발 스타일을 혼합하면 안됩니다.
만약 레거시 코드가 사전선언만 포함되어 있고, 
인라인 선언을 사용한 완벽한 리팩토링이 실현 가능하지 않다면   
이런 경우는 아마 레거시 스타일을 고수하는 게 두 스타일을 섞어서 사용하는 것보다 낫습니다.
스타일을 혼합하게 됐을 때 혼란을 일으킬 수 있는 상황들이 몇 가지 있습니다.
예를 들면 아래와 같습니다.
- Loop 구문을 사용할 때 `REF TO`와 `FIELD-SYMBOL`을 혼용하는 것
- `CONSTRUCTOR`를 호출할 때 `NEW`와 `CREATE OBJECT`를 혼용하는 것  
- 메서드에서 하나의 파라미터만을 return하거나 export할 때 
  `RETURNING`과 `EXPORTING`을 혼용하는 것 

우리는 리팩토링을 할 때 4단계의 좋은 결과들을 관찰했습니다. 

1. 팀과 함께 진행하세요. 새로운 스타일을 전달하고 설명하고, 
   프로젝트 팀의 모든 사람들이 그것에 동의하도록 해보세요.
   
2. 매일 일할 때 _보이 스카우트 규칙_ 을 따르도록 해보세요. 
   _항상 당신이 발견했을 때보다 조금이라도 코드를 깨끗하게 할 것_.
   "야영지 청소"와 같이 한꺼번에 많은 시간을 투자할 필요가 없고,
   단지 몇 분을 투자해 시간이 축적됨에 따라 어떻게 개선되는지를 관찰하면 됩니다. 

3. _깨끗한 섬_ 을 만들어 보세요. 수시로 작은 오브젝트나 컴포넌트를 선택하여
   모든 면에서 깨끗하게 하려고 노력해야 합니다. 
   이러한 섬들은 당신이 하고 있는 것의 많은 이점을 부각시켜 주고,
   더 나은 리팩토링을 가능하게 합니다.

4. 끊임 없이 대화하세요. 구식의 [파간 코드리뷰](https://en.wikipedia.org/wiki/Fagan_inspection) 를 선택하든,
   정보 세션을 개최하든, 당신이 가장 좋아하는 채팅 툴의 토론 게시판을 활용하든 관계 없습니다:
   이를 활용해 여러분의 경험과 학습에 대해 이야기할 필요가 있습니다.
   그리고 그것은 팀이 일반적인 이해를 성장시키는 데 도움을 줄 겁니다.  

### 자동으로 점검하는 방법

> [Clean ABAP](#clean-abap) > [내용](#내용) > [방법](#방법) > [이번 장](#자동으로-점검하는-방법)

[code pal for ABAP](https://github.com/SAP/code-pal-for-abap) 은
Clean ABAP에 대한 포괄적인 자동 검사 툴들을 제공합니다.

ABAP Test Cockpit, Code Inspector, Extended Check 및 Checkman은 
특정 문제를 찾는 데 도움이 될 수있는 몇 가지 검사를 제공합니다.

Code Inspector의 오프소스 중 하나인 [abapOpenChecks](https://github.com/larshp/abapOpenChecks) 
또한 안티-패턴 중 일부를 다룹니다.
(안티-패턴: 실제 많이 사용되는 패턴이지만 비효율적이거나 비생산적인 패턴, 역자 주)

[abaplint](https://github.com/abaplint/abaplint) 는 ABAP parser의 오픈소스 재구현입니다.
이것은 SAP 시스템없이 작동하며 abapGit을 사용하여 직렬화 된 코드에서 사용됩니다.
그리고 다양한 통합 툴들(GitHub Actions, Jenkins, 텍스트 편집기 등)을 제공하는데,
안티-패턴 중 몇몇 분야를 다루기도 하며, 서식 및 코드 규칙을 확인하는 데 사용할 수도 있습니다.

### 다른 가이드와 연결하는 방법

> [Clean ABAP](#clean-abap) > [내용](#내용) > [방법](#방법) > [이번 장](#다른-가이드와-연결하는-방법)

이 가이드는 Clean Code의 _spirit_ 을 따릅니다. 
이는 우리가 ABAP 프로그래밍 언어로 몇 가지를 조정했음을 의미합니다.

Our guide follows the _spirit_ of Clean Code,
meaning we adjusted some things to the ABAP programming language
예 : [관리 가능한 예외에 대해 CX_STATIC_CHECK 사용하기](#관리-가능한-예외-처리-구문에-대해-CX_STATIC_CHECK-구문을-사용하라).

내용 중 일부는 [ABAP 프로그래밍 가이드라인](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm)
에서 가져온 것인데 이는 대부분 호환됩니다.
어느 정도의 보정이 있긴 하지만, 그래도 항상 Clean Code 정신을 따릅니다.

이 가이드는 또한
[ABAP 개발을 위한 DSAG의 권장사항](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf),
을 참고하였는데, 대부분의 세부사항에서 우리가 더욱 더 정밀하다고 말할 수 있습니다.

이 가이드가 발행된 이후 Clean ABAP은 S/4 HANA에서 작업하는 
수백명의 개발자들을 포함하여 많은 SAP 사내 개발팀의 참조 가이드가 되었습니다.

### 동의하지 않는 방법 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [방법](#방법) > [이번 장](#동의하지-않는-방법)

우리는 이미 Clean Code에 대해 잘 알고 있거나 현재 ABAP 개발 중인 사용자들을 위해 이 가이드를 작성했습니다.
_특히 ABAP_ 에 Clean Code를 적용하는 방법을 공유하는 데 중점을 두었습니다. 

따라서 우리가 원본 책 및 관련 리소스와 동일한 수준으로 모든 개념을 소개하지는 않았다는 걸 아셨으면 합니다.
특히, 우리가 잘 설명하지 않음으로 인해서 여기 있는 내용에 동의하지 않는 경우가 발생할 수도 있을 겁니다.
섹션의 링크를 사용하여 해당 가이드의 배경에 대해서 공부하시기 바랍니다.

당신은 우리가 여기에서 말하는 것에 대해 자유롭게 토론하고 동의하지 않을 수 있습니다
Clean Code를 이루고 있는 것 중 하나는 바로 _팀 룰_ 입니다.
당신이 그것들을 버리기 전에, 공정한 기회를 주어야 합니다.

[CONTRIBUTING.md](../CONTRIBUTING.md) 는 당신이 이 가이드를 변경하거나 사소한 세부사항에서
벗어날 수 있는 법을 제안해 줄겁니다.

## 네이밍

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#네이밍)

### 설명이 포함된 이름을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#설명이-포함된-이름을-사용하라)

변수의 이름을 설정할 때 내용과 의미를 전달할 수 있도록 네이밍해야 합니다.

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

데이터 유형이나 기술 인코딩에 초점을 맞추지 마십시오.
그것들은 코드를 이해하는 데 거의 도움을 주지 못합니다.

```ABAP
" 잘못된 패턴
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[주석으로 잘못된 이름을 수정하지 마라](#주석으로-핑계를-대지-말고-코드를-변경하라)

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Use Intention-Revealing Names_ 를 참고하십시오.

### 솔루션 도메인이나 문제와 관련된 도메인 용어를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#솔루션-도메인이나-문제와-관련된-도메인-용어를-사용하라)

"큐"나 "트리" 같은 컴퓨터 과학 용어를 사용하는 솔루션 도메인을 찾거나
"계정"이나 "원장"과 같은 비즈니스 필드 용어를 사용하는 문제와 관련된 도메인을 찾으십시오.

비즈니스와 유사한 레이어는 문제 도메인에 따라 이름이 지정될 때 가장 잘 이해할 수 있습니다.
이는 특히 API 및 비즈니스 개체와 같이 도메인 기반 디자인으로 설계된 구성 요소의 경우 더 강조됩니다.

팩토리 클래스 및 추상 알고리즘과 같은 대부분의 기술적 기능을 제공하는 레이어는 솔루션 도메인에 따라 이름이 지정될 때 가장 잘 이해할 수 있습니다.

어떤 경우에도 자신의 언어를 구성하지 마십시오.
개발자, 제품 소유자, 파트너 및 고객간에 정보를 교환 할 수 있어야 합니다.
따라서 모든 것들이 사용자 정의된 사전없이 이해할 수 있는 이름으로 선택하십시오.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Use Solution Domain Names_ 를 참고하십시오.

### 복수형을 사용하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#복수형을-사용하라)

SAP에는 사물의 테이블을 단수로 명명하는 기존 관행이 있습니다.
예를 들어, 국가 테이블은 `country` 로 표기했었습니다.
외부 세계의 일반적인 경향은 사물의 복수형을 사용하는 것입니다.
따라서 우리는 `countries` 를 대신에 사용하는 것을 추천합니다.

> 이 조언은 주로 변수 및 속성과 같은 것을 대상으로합니다.
> 개발 객체의 경우 여러 의견이 모두 말이 되지만 서로 충돌하는 패턴이 있을 수 있습니다.
> 예를 들어 데이터베이스 테이블 ("transparent tables")의 이름을 단수로 지정하는 규칙이 그 예입니다.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Use Intention-Revealing Names_ 를 참고하십시오.

### 발음하기 쉬운 이름을 사용하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#발음하기-쉬운-이름을-사용하라)

우리는 사물에 대해 많이 생각하고 이야기하므로 발음 할 수있는 이름을 사용합니다.
예를 들어, `dobjt`와 같은 비밀스런 것보다`detection_object_types`를 선호합니다.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Use Pronounceable Names_ 를 참고하십시오.

### 약어를 피하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#약어를-피하라)

공간이 충분하면 약어를 사용하지 말고 풀네임을 사용하십시오.
길이 제한을 초과하는 경우에만 약어를 사용하십시오.

축약해야하는 경우 _중요하지 않은_ 단어로 시작하십시오.

약어는 언뜻 보기에는 효율적으로 보일 수 있지만 매우 빠르게 애매모호해집니다.
예를 들어, `cust`의 "cust"가 "customizing"인지 "customer"인지 "custom"인지 바로 알 수 있을까요?
세 단어 모두 SAP에서 정말 흔한 단어들입니다.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Make Meaningful Distinctions_ 를 참고하십시오.

### 모든 곳에서 동일한 약어를 사용하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#모든-곳에서-동일한-약어를-사용하라)

사람들은 관련 코드를 찾기 위해 키워드를 검색합니다.
동일한 것에 대해 동일한 약어를 사용하여 쉽게 검색하십시오.
예를 들어, "detection object type"이라는 단어를 사용할 땐
"dot", "dotype", "detobjtype"을 혼용해서 사용하는 대신 "dobjt"을 항상 사용하십시오.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Use Searchable Names_ 를 참고하십시오.

### 클래스에는 명사를 사용하고 메서드에는 동사를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#클래스에는-명사를-사용하고-메서드에는-동사를-사용하라)

명사 또는 명사구를 사용하여 클래스, 인터페이스 및 객체의 이름을 지정합니다:

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

동사 또는 동사구를 사용하여 메서드 이름 지정합니다:

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

`is_` 및 `has_`와 같은 동사로 논리 메서드를 시작하면 좋은 읽기 흐름이 생성됩니다:

```ABAP
IF is_empty( table ).
```

함수를 네이밍할때도 메서드와 같은 방식을 사용하길 추천합니다:

```ABAP
FUNCTION /clean/read_alerts
```

### 불필요한 단어를 피하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#불필요한-단어를-피하라)

노이즈 단어를 생략하세요.

```ABAP
account  " instead of account_data
alert    " instead of alert_object
```

또는 실제로 의미를 특정하는 항목으로 단어를 바꾸세요.

```ABAP
user_preferences          " user_info 대신
response_time_in_seconds  " response_time_variable 대신
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Make Meaningful Distinctions_ 를 참고하십시오.

### 개념당 하나의 단어를 선택하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#개념당-하나의-단어를-선택하라)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

개념에 대한 용어를 선택하고 고수하십시오; 다른 동의어를 섞지 마십시오.
동의어는 개발자가 거기에 없는 차이점을 찾는 데 시간을 낭비하게 만들 것입니다.

```ABAP
" 잘못된 패턴
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Pick One Word per Concept_ 를 참고하십시오.

### 의미 있는 경우에만 패턴 이름을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#의미-있는-경우에만-패턴-이름을-사용하라)

특별한 경우가 아니라면 클래스와 인터페이스에 소프트웨어 디자인 패턴의 이름을 사용하지 마십시오.
예를 들어 실제로 팩토리 디자인 패턴을 구현하지 않는 한 클래스를 `file_factory`라고 부르지 마십시오.
가장 일반적인 패턴은 다음과 같습니다:
[singleton](https://en.wikipedia.org/wiki/Singleton_pattern),
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern),
[facade](https://en.wikipedia.org/wiki/Facade_pattern),
[composite](https://en.wikipedia.org/wiki/Composite_pattern),
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern),
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern),
[observer](https://en.wikipedia.org/wiki/Observer_pattern), 그리고
[strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 2 : Meaningful Names: Avoid Disinformation_ 를 참고하십시오.

### 헝가리안 표기법과 접두사 같은 불필요한 인코딩을 피하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [네이밍](#네이밍) > [이번 장](#헝가리안-표기법과-접두사-같은-불필요한-인코딩을-피하라)

우리는 _모든_ 인코딩 접두사를 제거하는 것이 좋다고 생각합니다.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

이는 불필요하게 긴 것을 대체합니다.

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> [Avoid Encodings](sub-sections/AvoidEncodings.md)
> 에서는 그 근거를 깊이 있게 설명합니다.

## 언어

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#언어)

### 레가시를 신경 써라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [언어](#언어) > [이번 장](#레가시를-신경-써라)

이전 ABAP 릴리스용으로 코딩하는 경우 이 가이드의 조언을 주의해서 따르십시오.
아래의 많은 권장 사항은 이전 ABAP 릴리스에서 지원되지 않을 수 있는 비교적 새로운 구문 및 구문을 사용합니다.
지원해야 하는 가장 오래된 릴리스에서 따라야 하는 지침을 확인하십시오.
그렇다고 Clean Code 전체를 단순히 적용하지 말라는 것이 아닙니다 -
대부분의 규칙(예) 네이밍, 주석 달기)들은 '모든' ABAP 버전에 적용 가능합니다.

### 수행속도를 신경 써라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [언어](#언어) > [이번 장](#수행속도를-신경-써라)

이전 ABAP 릴리스용으로 코딩하는 경우 이 가이드의 조언을 주의해서 따르십시오:
Clean Code의 일부 측면은 작업을 느리게 만들거나(더 많은 메서드 호출) 더 많은 메모리를 사용(더 많은 오브젝트)할 수 있습니다.
ABAP에는 이를 뒷받침하는 몇 가지 특수 기능이 있습니다. 예를 들어 메서드를 호출할 때 
데이터 유형을 비교하여 하나의 큰 메서드를 여러 하위 메서드로 분할하면 코드가 느려질 수 있습니다. 

그러나 모호한 두려움을 기반으로 최적화를 하지 않는 것은 좋지 못합니다.
대다수의 규칙(예: 이름 지정, 주석 달기)은 전혀 부정적인 영향을 미치지 않습니다.
깨끗하고 객체 지향적인 방식으로 빌드를 시도하십시오.
너무 느린 경우 성능 측정을 수행하십시오.
그런 다음에는 선택한 규칙을 폐기하기 위해 사실에 기반한 결정을 내려야 합니다.

더 많은 정보들을 얻기 위해서는 
[Martin Fowler's _Refactoring_](https://martinfowler.com/books/refactoring.html) 의 두번째 장의 일부를 참조하십시오:

일반적인 응용 프로그램에서 대부분의 런타임은 코드의 아주 작은 부분에서 시간이 소요됩니다.
전체 런타임의 90%가 10%의 코드에서 발생하는데, 
특히 ABAP에서는 런타임의 대부분이 데이터베이스 관련한 시간일 가능성이 높습니다.
따라서 _모든_ 코드를 효율적으로 만들기 위해 상당한 노력을 기울이는 것은 리소스를 잘 사용하는 것이 아닙니다.
성능을 무시하자는 것이 아니라, 차라리 초기 개발 기간동안에 깨끗하고 잘 구조화된 코드에 집중하고
프로파일러를 사용하여 최적화할 중요한 영역을 식별하자는 의도입니다.

사실, 우리는 그러한 접근 방식이 보다 집중된 최적화를 위한 노력이기 때문에 성능에 긍정적인 영향을 미칠 것이라고 주장합니다.
또한 이는 성능 병목 현상을 더 쉽게 식별하고, 더 쉽게 잘 구조화된 코드를 리팩토링하고 조정할 것입니다.

### 절차지향적 프로그래밍보다 객체지향적 프로그래밍을 선호하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [언어](#언어) > [이번 장](#절차지향적-프로그래밍보다-객체지향적-프로그래밍을-선호하라)


객체 지향 프로그램(클래스, 인터페이스)은 더 쉽게 세분화될 수 있고
절차 코드(함수, 프로그램)보다 쉽게 리팩토링 및 테스트할 수 있습니다.
절차 지향적 객체를 제공해야 하는 상황들이 있지만(RFC를 위한 함수, 트랜잭션을 위한 프로그램),
이러한 객체는 실제 기능을 제공하는 해당 클래스를 호출하는 것 이상을 수행해야 합니다.
(역자주: RFC 호출 이후에도 추가적인 단계들이 더 있을 수 있다는 뜻)

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> [Function Groups vs. Classes](sub-sections/FunctionGroupsVsClasses.md)
> describes the differences in detail.

### 절차지향적 언어 구성보다 기능적인 구성을 선호하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [언어](#언어) > [이번 장](#절차지향적-언어-구성보다-기능적인-구성을-선호하라)

아래와 같은 코드들이 좀 더 요즘 프로그래머들에게 친숙합니다.

```ABAP
DATA(variable) = 'A'.
" 'A'를 variable에 대입합니다.

DATA(uppercase) = to_upper( lowercase ).
" lowercase를 대문자로 치환한 후 uppercase에 대입합니다.

index += 1.         " >= NW 7.54
index = index + 1.  " < NW 7.54
" index에 1을 더합니다.

DATA(object) = NEW /clean/my_class( ).
" /dirty/my_class type의 객체를 생성하여 object에 대입합니다.

result = VALUE #( FOR row IN input ( row-text ) ).
" LOOP AT input INTO DATA(row).
"  INSERT row-text INTO TABLE result.
" ENDLOOP.

DATA(line) = value_pairs[ name = 'A' ]. " entry가 반드시 있어야 함
DATA(line) = VALUE #( value_pairs[ name = 'A' ] OPTIONAL ). " entry가 없어도 됨
" READ TABLE value_pairs INTO DATA(line) WITH KEY name = 'A'.

DATA(exists) = xsdbool( line_exists( value_pairs[ name = 'A' ] ) ).
IF line_exists( value_pairs[ name = 'A' ] ).
" READ TABLE value_pairs TRANSPORTING NO FIELDS WITH KEY name = 'A'.
" DATA(exists) = xsdbool( sy-subrc = 0 ).
```

아래 나오는 세부 규칙들은 위와 같은 일반적인 조언들을 구체적으로 반복한 것입니다.

### 쓸모 없는 언어 요소를 피하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [언어](#언어) > [이번 장](#쓸모-없는-언어-요소를-피하라)

ABAP 버전을 업그레이드할 때 사용되지 않는 언어 요소를 확인하고 사용을 자제하십시오.

예를 들어, 아래의 문장에 나오는 '@' 이스케이프된 "호스트" 변수는
프로그램 변수와 데이터베이스에서 사용하는 칼럼이 무엇인지 더 명확하게 해줍니다.

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

아래를 참고하십시오.
[구식의 이스케이프되지 않은 형식](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm)

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```
새로운 대안은 코드의 가독성을 향상시키고 최근의 프로그래밍 패러다임과의 설계 충돌을 줄이는 경향이 있어 
이러한 대안으로 전환하면 코드가 자동으로 정리될 수 있습니다.

일을 계속하는 동안, 사용되지 않는 요소는 처리 속도 및 메모리 소비 측면에서 최적화를 늦출 수 있습니다.

최신의 언어 요소들을 사용하면, 
SAP 교육에서 가르치지도 않는 구식 구조들을 
더 이상 공부할 필요가 없는 젊은 ABAPer들이 쉽게 정착할 수 있습니다.

SAP NetWeaver 문서에는 사용되지 않는 언어 요소를 나열하는 안정적인 섹션이 있습니다.
아래와 같습니다.
[NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm).

### 디자인 패턴을 현명하게 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [언어](#언어) > [이번 장](#디자인-패턴을-현명하게-사용하라)

적절한 곳에서 디자인 패턴을 사용하면 눈에 띄는 이점들이 보입니다.
단지 그것만을 위해 모든 곳에 디자인 패턴을 적용하지 마십시오.

## 상수

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#상수)

### 매직넘버 대신에 상수를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [상수](#상수) > [이번 장](#매직넘버-대신에-상수를-사용하라)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date. 
```

이 아래 구문보다 더 깔끔합니다.

```ABAP
" 잘못된 패턴
IF abap_type = 'D'.
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 17 : Smells and Heuristics: G25:
> Replace Magic Numbers with Named Constants_ 를 참고하십시오.

### 상수 인터페이스보다 열거체를 선호하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [상수](#상수) > [이번 장](#상수-인터페이스보다-열거체를-선호하라)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

또는

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

아래와 같은 패턴들은
관련 없는 것들을 섞거나, 상수 컬렉션으로 "구현"될 수 있다는 결론으로 사람들을 오해 시킬 수 있습니다.

```ABAP
" 잘못된 패턴
INTERFACE /dirty/common_constants.
  CONSTANTS:
    warning      TYPE symsgty VALUE 'W',
    transitional TYPE i       VALUE 1,
    error        TYPE symsgty VALUE 'E',
    persisted    TYPE i       VALUE 2.
ENDINTERFACE.
```

> [Enumerations](sub-sections/Enumerations.md) 에서는 일반적인 열거체 패턴을 설명하고, 그들의 장단점을 알려줍니다.
> 
> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 17 : Smells and Heuristics: J3: Constants versus Enums_ 를 참고하십시오.

### 열거체를 사용하지 않는다면 상수들을 그룹화하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [상수](#상수) > [이번 장](#열거체를-사용하지-않는다면-상수들을-그룹화하라)

예를 들어 아래의 인터페이스에서와 같이 느슨한 방식으로 상수를 수집하는 경우 그룹화하십시오:

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

이는 아래의 패턴보다 관계를 더욱 명확하게 해줍니다:

```ABAP
" 잘못된 패턴
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

그룹을 사용하면 입력 유효성 검사와 같은 그룹 단위 액세스도 허용합니다:

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

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 17 : Smells and Heuristics: G27: Structure over Convention_ 를 참고하십시오.

## 변수

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#변수)

### 변수는 사용하는 위치에 최대한 가까이 선언하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [변수](#변수) > [이번 장](#변수는-사용하는-위치에-최대한-가까이-선언하라)

이 지침을 따르면 메서드가 너무 짧아서(3-5개 줄) 처음에 인라인으로 변수를 선언하는 것이 더 자연스럽게 보입니다.

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

메서드 시작 부분에 별도의 `DATA` 섹션으로 변수를 선언하는 것은 다음과 같습니다.

```ABAP
" 잘못된 패턴
METHOD do_something.
  DATA:
    name   TYPE seoclsname,
    reader TYPE REF TO /dirty/reader.
  name = 'something'.
  reader = /dirty/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 5 : Formatting: Vertical Distance: Variable Declarations_ 를 참고하십시오.

### 분기점에서 인라인을 선언하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [변수](#변수) > [이번 장](#분기점에서-인라인을-선언하지-마라)

```ABAP
" 잘못된 패턴
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```
이것은 ABAP이 인라인 선언을 메서드의 시작 부분에 있는 것처럼 처리하기 때문에 잘 작동합니다.
그러나 이것은 실제 개발자들에게는 매우 혼란스러울 수가 있는데, 
특히 메서드가 길고 선언을 즉시 발견하지 못하는 경우에는 더 그렇습니다.
이 경우 인라인 선언을 중단하고 맨 윗줄에 코드를 작성합니다.

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 5 : Formatting: Vertical Distance: Variable Declarations_  를 참고하십시오.

### 선행 선언을 서로 묶지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [변수](#변수) > [이번 장](#선행-선언을-서로-묶지-마라)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO reader.
```

체이닝은 정의된 변수가 논리적 수준에서 관련되어 있음을 나타냅니다.
일관되게 사용하려면 연결된 모든 변수가 함께 속하는지 확인해야 하는데,
이는 변수를 추가하기 위해서 추가적인 체인 그룹을 도입해야한다는 것을 의미합니다.
이것은 가능하지만, 보통은 그리 할 만한 가치가 없습니다.

체이닝은 또한 재포맷과 리팩토링을 불필요하게 복잡하게 만듭니다.
각 행이 다르게 보이고 변경하려면 콜론, 점 및 쉼표를 계속 넣어야하므로 역시나 그럴만한 가치가 없습니다.

```ABAP
" 잘못된 패턴
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO reader.
```

> [Type절은 정렬하지 마라](#Type절은-정렬하지-마라) 도 참조하십시오.   
> 데이터 선언에서 체이닝을 사용하는 경우 함께 속하는 각 변수 그룹에 대해 하나의 체인을 사용합니다.

### FIELD-SYMBOL보다 REF TO를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [변수](#변수) > [이번 장](#FIELD-SYMBOL보다-REF-TO를-사용하라)

> [is being challenged](https://github.com/SAP/styleguides/issues/115) 를 참조하세요.
> `FIELD-SYMBOL` 은 내부 테이블을 조회할 때 상당히 빠른 것 같지만
> 아래와 같은 경우에 `REF TO` 를 사용하는 것보다도 성능이 저하될 수 있습니다. 

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

동일한 구문에 `FIELD-SYMBOL` 을 사용한 예시

```ABAP
" 잘못된 패턴
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

`FIELD-SYMBOL`이 사용되는 곳은 아래와 같음 

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

코드 리뷰는 사람들이 둘 사이에서 임의로 선택하는 경향이 있음을 보여줍니다,
이유를 들어보면 "그냥", "우리가 항상 그런 식으로 반복하기 때문에" 또는 "특별한 이유 없이" 등 아무 것도 아닌 것들입니다.
임의의 선택은 개발자가 '왜 하나가 다른 하나를 대신해서 사용되는가' 와 같은 무의미한 질문들로 시간을 낭비하게 만듭니다.
따라서 근거가 있고 정확한 결정으로 대체되어야 합니다.
우리의 권장 사항은 다음과 같은 추론에 근거합니다.

- Field symbols 은 구조의 구성 요소에 동적으로 액세스하는 등의 references 가 할 수 없는 몇 가지 작업을 수행할 수 있습니다.
  반면에, references 는 동적으로 유형이 지정된 데이터 구조를 구성하는 등의 field symbols 가 할 수 없는 몇 가지 작업을 수행할 수 있습니다.
  요약하자면, 하나만을 계속적으로 사용하는 것은 불가능합니다.
  
- 객체 지향 ABAP 에서 references 는 도처에 있으며 피할 수 없습니다. 예를 들어, 모든 객체는 `REF TO <class-name>` 입니다.
  이와는 대조적으로 field symbols 은 필드 기호는 동적 입력과 관련된 몇몇의 특수한 경우에만 엄격하게 필요합니다.
  따라서 references 는 모든 객체 지향 프로그램에서 자연스러운 선호도를 형성합니다.

- Field symbols 은 references 보다는 짧지만, 결과적으로 절약되는 메모리는 너무 작아서 안전하게 무시할 수 있습니다.
  유사하게 속도 또한 문제가 되지 않습니다.
  결과적으로 성능과 관련하여 둘 중 하나를 선호할 이유가 없습니다.
  
> 아래에서 더 많은 정보를 얻어보세요.  
> [_Accessing Data Objects Dynamically_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm).

## 테이블

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#테이블)

### 올바른 테이블 유형을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#올바른-테이블-유형을-사용하라)

- 당신은 일반적으로 **큰 테이블**에서 `HASHED` 테이블을 사용하는데 이는
  **한 단계로 작성되었고**, **수정되지 않으며**, **키로 자주 읽는** 테이블입니다.
  고유한 메모리 및 처리 오버헤드는 해시 테이블이 많은 양의 데이터와 많은 읽기 엑세스들을 생기게 만듭니다.
  테이블 내용이 변경될 때마다 해시를 다시 계산해야 하므로 너무 자주 수정되는 테이블에는 이를 사용하지 마십시오.

- 당신은 일반적으로 **큰 테이블**에서 `SORTED` 테이블을 사용하는데 이는  
  **항상 정렬이 필요하고**, **비트 단위로 채워지거나 수정이 필요한 항목일 수도 있으며**,
  **하나 이상의 전체 또는 부분 키로 자주 읽거나**, **특정 순서로 처리되는** 테이블입니다.
  콘텐츠를 추가, 변경 또는 제거하려면 올바른 삽입 지점을 찾아야 하지만 테이블 인덱스의 나머지 부분을 조정할 필요는 없습니다.
  정렬된 테이블은 많은 수의 읽기 액세스에 대해서만 값을 보여줍니다.  

- 당신은 일반적으로 **작은 테이블**에서 `STANDARD` 테이블을 사용하는데 이는
  인덱싱을 하면서 많은 오버헤드를 생성하고, 
  **배열**과 같이 행의 순서에 전혀 신경 쓰지 않거나 행이 추가된 순서대로 정확하게 처리하려고 할 때 사용하는 테이블입니다. 
  또한 `SORT` 나 `BINARY SEARCH` 와 같이 테이블에 대한 다른 엑세스가 필요한 경우에도 자주 쓰입니다.

> 이것은 대략적인 지침일 뿐입니다.
> 아래에서 더 많은 정보를 얻어보세요.  
> [_Selection of Table Category_ in the ABAP Language Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm).

### 기본 키 사용을 피하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#기본-키-사용을-피하라)

```ABAP
" 잘못된 패턴
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

기본 키는 종종 새로운 기능 명령문이 작동하도록 하기 위해 추가됩니다.
실제로 키 자체는 일반적으로 불필요하고 자원을 낭비합니다.
숫자 데이터 유형을 무시하기 때문에 덮어 감추고 싶은 실수로 이어질 수도 있습니다.
명시적 필드 목록이 없는 `SORT` 및 `DELETE ADJACENT` 문은 내부 테이블의 기본 키에 의존하는데,
이는 키의 구성 요소로 숫자 필드 특히 `READ TABLE ... BINARY` 등과 함께 사용합니다.

핵심 구성요소를 명시적으로 지정하거나

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

또는 키가 전혀 필요하지 않은 경우 'EMPTY KEY' 를 사용하세요.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> 아래에서 더 많은 정보를 얻어보세요.  
> [Horst Keller's blog on _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)   
> **주의:** 명시적 정렬된 필드들이 없는 `EMPTY KEY` 를 사용하는 내부 테이블의 'SORT' 구문은 하나도 정렬이 되지 않습니다.
> 그러나, 키의 비어 있음을 정적으로 결정할 수 있는 경우 구문 경고가 발생합니다.

### APPEND TO 보다 INSERT INTO TABLE을 사용하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#APPEND-TO-보다-INSERT-INTO-TABLE을-사용하라)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` 은 모든 테이블 및 키 유형에서 작동합니다.
따라서 성능 요구 사항이 변경되는 경우 테이블의 유형 및 키 정의를 쉽게 리팩토링할 수 있습니다.

만약 당신이 추가하고자 하는 항목이 반드시 마지막 행이어야함을 강조하기를 원한다면,
배열과 같은 방식으로 `STANDARD` 테이블에서 `APPEND TO` 구문을 사용하십시오.

### READ TABLE이나 LOOP AT보다 LINE EXISTS 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#READ-TABLE이나-LOOP-AT보다-LINE-EXISTS-구문을-사용하라)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

와 같이 사용하면 의도를 아래의 구문들보다 명확하고 짧게 표현할 수 있다.

```ABAP
" 잘못된 패턴
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

또는 

```ABAP
" 잘못된 패턴
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### LOOP AT보다 READ TABLE 구문을 사용하라 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#LOOP-AT보다-READ-TABLE-구문을-사용하라)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

와 같이 사용하면 의도를 아래의 구문들보다 명확하고 짧게 표현할 수 있다.

```ABAP
" 잘못된 패턴
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

또는

```ABAP
" 잘못된 패턴
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### 중첩된 IF문보다 LOOP AT 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#중첩된-IF문보다-LOOP-AT-구문을-사용하라)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

expresses the intent clearer and shorter than

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### 불필요한 테이블 읽는 것을 피하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테이블](#테이블) > [이번 장](#불필요한-테이블-읽는-것을-피하라)

당신이 하나의 행을 _예상_ 할 경우에, 한번에 읽고 예외처리를 하십시오.

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

아래와 같이 이중 읽기로 주요 흐름들을 늦추는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = my_table[ key = input ].
```

> 성능 향상과 더불어 이것은 아래 섹션의 특별한 변형의 한 형태입니다.
> [HAPPY-PATH 혹은 에러 처리 둘 중 하나에 집중하라](#HAPPY-PATH-혹은-에러-처리-둘-중-하나에-집중하라).

## 문자열

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#문자열)

### 리터럴을 정의하기 위해 억음부호를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [문자열](#문자열) > [이번 장](#리터럴을-정의하기-위해-억음부호를-사용하라)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

개발자가 `CHAR` 또는 `STRING` 중에서 어떤 것을 써야 하는지 모를 때 혼란을 가중시킬 수 있으니 굳이 `'` 을 사용하지 마십시오.

```ABAP
" 잘못된 패턴
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|`는 일반적으로 괜찮지만 `CONSTANTS`에는 사용할 수 없으며 고정 값을 지정할 때 불필요한 오버헤드를 야기합니다.

```ABAP
" 잘못된 패턴
DATA(some_string) = |ABC|.
```

### 텍스트를 이을 때는 파이프 문자를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [문자열](#문자열) > [이번 장](#텍스트를-이을-때는-파이프-문자를-사용하라)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

문자열 템플릿은 리터럴과 변수를 더욱 강조하는데, 특히 텍스트에 여러 변수를 포함하는 경우에 더 그렇습니다.

```ABAP
" 잘못된 패턴
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Booleans

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#booleans)

### Boolean을 현명하게 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [Booleans](#booleans) > [이번 장](#Boolean을-현명하게-사용하라)

Boolean이 자연스러운 선택인 것처럼 보이는 경우가 종종 있습니다.

```ABAP
" 잘못된 패턴
is_archived = abap_true.
```

관점의 변화가 시작되기 전까지는 우리는 열거형을 선택했어야 됐습니다.

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

일반적으로 Boolean은 오브젝트의 유형을 구별하는 데에는 좋지 않습니다.
왜냐하면 거의 항상 하나 또는 다른 하나가 아닌 경우가 발생하기 때문입니다.

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

[Boolean 파라미터를 사용하는대신에 메서드를 분할하라](#Boolean-파라미터를-사용하는-대신에-메서드를-분할하라)
여기에서도 항상 boolean 파라미터가 도전 받는 이유에 대해서 설명합니다.

> 아래에서 더 많은 정보를 얻어보세요.  
> [1](http://www.beyondcode.org/articles/booleanVariables.html)

### Boolean에서는 ABAP_BOOL을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [Booleans](#booleans) > [이번 장](#Boolean에서는-ABAP_BOOL을-사용하라)

```ABAP
DATA has_entries TYPE abap_bool.
```

데이터 유형 `char1`을 사용하지 마십시오.
기술적으로 호환되지만 Boolean 변수를 다루는 게 애매할 수 있습니다.

또한 종종 이상한 부작용이 있기 때문에 다른 부울 유형을 피하십시오.
예를 들어 `boolean`은 미묘한 프로그래밍 오류를 일으키는 세 번째 값 `undefined` 를 지원합니다.

어떤 경우에는 DynPro 필드와 같은 데이터 사전 요소가 필요할 수 있습니다.
`abap_bool`은 데이터 사전이 아닌 `abap` 유형 풀에 정의되어 있으므로 여기에서 사용할 수 없습니다.
이 경우 `boole_d` 또는 `xfeld`에 의존합니다.
사용자 정의 설명이 필요한 경우 고유한 데이터 요소를 작성하십시오.

> ABAP 은 범용 boolean 데이터 유형이 제공되지 않는 단일 프로그래밍 언어일 수 있습니다.
> 이 권장 사항은 ABAP 프로그래밍 지침을 기반으로 합니다.

### 서로를 비교할 때는 ABAP_TRUE와 ABAP_FALSE를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [Booleans](#booleans) > [이번 장](#서로를-비교할-때는-ABAP_TRUE와-ABAP_FALSE를-사용하라)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

`'X'` 및 `' '` 또는 `공백`에 해당하는 문자를 사용하지 마십시오.
그들은 이것이 boolean 변수라는 것을 알기 어렵게 만듭니다.

```ABAP
" 잘못된 패턴
has_entries = 'X'.
IF has_entries = space.
```

`INITIAL`과의 비교를 피하십시오. 개발자가 `abap_bool`의 기본값이 `abap_false`임을 기억하도록 합니다.

```ABAP
" 잘못된 패턴
IF has_entries IS NOT INITIAL.
```

> ABAP은 true 및 false에 대한 내장 "상수"와 함께 제공되지 않는 단일 프로그래밍 언어일 수 있습니다.
> 이 권장 사항은 ABAP 프로그래밍 지침을 기반으로 합니다.

### Boolean 변수를 설정할 때는 XSDBOOL을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [Booleans](#booleans) > [이번 장](#Boolean-변수를-설정할-때는-XSDBOOL을-사용하라)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

동일한 의미의 `IF`-`THEN`-`ELSE`는 코드가 훨씬 더 깁니다.

```ABAP
" 잘못된 패턴
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool`은 `char1`을 직접 생성하므로 가장 적합한 방법인데, 이는 Boolean 유형 `abap_bool`에 가장 잘 맞습니다.
동일한 함수 `boolc` 및 `boolx` 는 다른 유형들을 생성하고 불필요한 암시적 유형 변환을 추가합니다.

우리는 `xsdbool`이라는 이름이 불길하고 오해의 소지가 있다는 데 동의합니다.
결국 우리는 "xsd" 접두사가 제안하는 "XML 스키마 정의" 부분에 전혀 관심이 없습니다.

`xsdbool`의 가능한 대안은 `COND` 삼항 형식입니다.
구문은 직관적이지만 `THEN abap_true` 세그먼트를 불필요하게 반복하기 때문에 조금 더 길고,
그리고 abap_bool의 기본값이 `abap_false` 이라는 것에 대한 지식이 필요합니다.
이것이 우리가 2차 솔루션으로만 제안하는 이유입니다.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## 컨디션

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#컨디션)

### 컨디션을 긍정문으로 작성하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [컨디션](#컨디션)> [이번 장](#컨디션을-긍정문으로-작성하라)

```ABAP
IF has_entries = abap_true.
```

동일한 문장을 반대로 하면 이해하기 얼마나 어려운지 비교해보세요:

```ABAP
" 잘못된 패턴
IF has_no_entries = abap_false.
```

"시도" 섹션이라는 타이틀은 당신이 [IF분기에서 빈 곳을 만들지 마라](#IF분기에서-빈-곳을-만들지-마라) 처럼 부정적인 구문을 사용하면 안된다는 것을 의미합니다.

```ABAP
" 잘못된 패턴
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 17 : Smells and Heuristics: G29: Avoid Negative Conditionals_ 를 참고하십시오.

### NOT IS보다 IS NOT을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [컨디션](#컨디션)> [이번 장](#NOT-IS보다-IS-NOT을-사용하라)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

부정문은 논리적으로는 동일하지만 이해하기 어렵게 만들기 때문에 "발상의 전환"이 필요합니다.

```ABAP
" 잘못된 패턴
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> [컨디션을 긍정문으로 작성하라](#컨디션을-긍정문으로-작성하라)의 보다 구체적인 변형입니다.
또한 ABAP 프로그래밍 지침의 [대체 언어 구성](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenalternative_langu_guidl.htm) 섹션에 설명되어 있습니다.

### 복잡한 컨디션들을 쪼개는 것을 고려하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [컨디션](#컨디션)> [이번 장](#복잡한-컨디션들을-쪼개는-것을-고려하라)

조건을 구성하는 기본 요소로 분해하면 조건이 더 쉬워질 수 있습니다:

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

모든 조건들을 하나에 몰아서 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> ABAP 개발 도구를 사용하여 위와 같이 조건을 빠르게 추출하고 변수를 생성하십시오.

### 복잡한 컨디션들을 메서드화하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [컨디션](#컨디션)> [이번 장](#복잡한-컨디션들을-메서드화하라)

복잡한 조건을 고유한 메서드으로 추출하는 것은 거의 항상 좋은 생각입니다:

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

## 조건문

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#조건문)

### IF분기에서 빈 곳을 만들지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [조건문](#조건문) > [이번 장](#IF분기에서-빈-곳을-만들지-마라)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

위와 같이 사용하는 것이 아래보다 훨씬 더 짧고 깔끔합니다.

```ABAP
" 잘못된 패턴
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### 여러 대안이 있다면 ELSE IF 대신에 CASE를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [조건문](#조건문) > [이번 장](#여러-대안이-있다면-ELSE-IF-대신에-CASE를-사용하라)

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

`CASE`는 여러 대안들의 집합을 쉽게 볼 수 있도록 합니다.
일련의 후속 평가 조건 대신 다른 마이크로프로세서 명령으로 변환할 수 있기 때문에 일련의 `IF`보다 빠를 수 있습니다.
분별력 있는 변수를 계속 반복하지 않고도 새로운 사례를 빠르게 도입할 수 있습니다.
이 명령문은 실수로 `IF`-`ELSEIF`를 중첩할 때 발생할 수 있는 일부 오류도 방지합니다.

```ABAP
" 잘못된 패턴
IF type = type-some_type.
  " ...
ELSEIF type = type-some_other_type.
  " ...
ELSE.
  RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

### 분기를 최대한 적게 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [조건문](#조건문) > [이번 장](#분기를-최대한-적게-하라)

```ABAP
" 잘못된 패턴
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

중첩된 `IF`는 매우 이해하기가 어렵고, 완전한 적용을 위해 기하급수적인 테스트 케이스가 필요합니다.

의사 결정 트리는 일반적으로 하위 메서드를 구성하고 Boolean 변수를 도입하여 분리할 수 있습니다.

다음과 같은 IF를 병합하여 다른 경우를 단순화할 수 있습니다.

```ABAP
IF <this> AND <that>.
```

불필요하게 if문을 중첩하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
IF <this>.
  IF <that>.
```

## 정규식

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#정규식)

### 정규식 대신에 간단한 메서드를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [정규식](#정규식) > [이번 장](#정규식-대신에-간단한-메서드를-사용하라)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

정규식은 매우 이해하기가 어렵습니다.
간단한 경우는 일반적으로 없는 경우가 더 쉽습니다.

정규식은 또한 식 트리로 구문 분석되고 런타임에 실행 가능한 매처로 컴파일되어야 하기 때문에 일반적으로 더 많은 메모리와 처리 시간을 소비합니다.
간단한 솔루션은 간단한 루프와 임시 변수로 할 수 있습니다.

### 정규식 대신에 기본적인 체크를 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [정규식](#정규식) > [이번 장](#정규식-대신에-기본적인-체크를-하라)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

아래와 같이 작성하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> 주변에 정규식이 있을 때, DRY(Don't-Repeat-Yourself) 원칙을 신경쓰지 않게 되는 자연스러운 경향이 있는 것 같습니다. (역자주 : DRY 원칙 - 똑같은 일을 두 번하지 말것!)
> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 17: Smells and Heuristics: General: G5: Duplication_ 과 비교해 보십시오.

### 복잡한 정규식들을 조합하는 것을 고려하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [정규식](#정규식) > [이번 장](#복잡한-정규식들을-조합하는-것을-고려하라)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

일부 복잡한 정규식은 기본적인 부분으로 구성되는 방식을 개발자에게 보여주면 더 쉬워집니다.

## 클래스

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#클래스)

### 객체 지향 클래스

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [이번 장](#객체-지향-클래스)

#### 정적인 클래스보다 오브젝트를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [객체 지향 클래스](#객체-지향-클래스) > [이번 장](#정적인-클래스보다-오브젝트를-사용하라)

우선, 정적 클래스는 객체 지향으로 얻은 모든 이점을 포기합니다. 특히 단위 테스트에서 생산적인 종속성을 테스트 이중으로 대체하는 것을 거의 불가능하게 만듭니다.

클래스 또는 메서드를 정적으로 만들지 여부를 생각하면 대답은 거의 항상 이렇습니다 : 아니오

이 규칙에 대해 허용되는 한 가지 예외는 일반 유형의 유틸 클래스입니다.
그들의 방법을 사용하면 특정 ABAP 유형과 더 쉽게 상호 작용할 수 있습니다.
그것들은 완전히 상태를 저장하지 않을 뿐만 아니라, 너무 기본적이어서 ABAP 문이나 내장 함수처럼 보입니다.
차별화된 요소는 그들의 소비자들이 그들을 그들 자신의 코드에 너무 단단히 얽매이게 만드는데 그들은 실제로는 단위 테스트에서 그들을 mocking하고 싶어하지 않습니다.

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

#### 상속 대신에 구성을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [객체 지향 클래스](#객체-지향-클래스) > [이번 장](#상속-대신에-구성을-사용하라)

가급적 상속 클래스를  지양하십시오. 대신 구성을 선호하십시오.

클린 상속은 [리스코프 대체 원리](https://en.wikipedia.org/wiki/Liskov_substitution_principle)와 같은 규칙을 준수해야 하기 때문에 설계하기 어렵습니다.
또한 사람들이 계층 구조 이면의 기본 원칙을 깨닫고 소화해야 하기 때문에 이해하기 어렵습니다.
상속은 메서드를 하위 클래스에서만 사용할 수 있게 하는 경향이 있기 때문에 재사용을 줄입니다.
또한 멤버를 이동하거나 변경하려면 전체 계층 구조 트리를 변경해야 하는 경향이 있기 때문에 리팩토링이 복잡해집니다.

구성이란 각각 하나의 특정 목적을 수행하는 작고 독립적인 개체를 디자인하는 것을 의미합니다.
이러한 개체는 단순한 위임 및 퍼사드 패턴에 의해 보다 복잡한 개체로 재결합될 수 있습니다. (역자주 : 퍼사드 패턴 관련 링크: https://invincibletyphoon.tistory.com/22)
구성은 더 많은 클래스를 생성할 수 있지만 그렇지 않으면 더 이상의 단점이 없습니다.

이 규칙이 올바른 위치에서 상속을 사용할 수 있도록 해주십시오.
[복잡한 디자인 패턴](https://en.wikipedia.org/wiki/Composite_pattern)과 같은 좋은 상속 응용 프로그램이 있습니다.
각자 자신의 경우에 상속이 실제로 단점보다 더 많은 장점을 제공하는지 비판적으로 자문하십시오.
확실하지 않은 경우 일반적으로 구성이 더 안전한 선택입니다.

> [Interfaces vs. abstract classes](sub-sections/InterfacesVsAbstractClasses.md)
자세한 내용은 위 링크를 참조하세요.

#### 동일한 클래스에서 상태 저장과 비저장을 혼재하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [객체 지향 클래스](#객체-지향-클래스) > [이번 장](#동일한-클래스에서-상태-저장과-비저장을-혼재하지-마라)

동일한 클래스에서 상태 비저장 및 상태 저장 프로그래밍 패러다임을 혼합하지 마십시오.

상태를 저장하지 않는 프로그래밍에서 메서드는 _별다른 사이드이펙트 없이_  입력을 받고 출력을 하므로 메서드가 호출되는 시기와 순서에 관계없이 동일한 결과를 생성합니다.

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

상태를 저장하는 프로그래밍에서 우리는 메서드를 통해 객체의 내부 상태를 조작합니다. 즉, _사이드 이펙트가 가득합니다_.

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

두 패러다임 모두 괜찮고 알맞는 응용 프로그램들을 가지고 있습니다.
그러나 동일한 객체에서 이들을 _혼합_ 하면 이해하기 어렵고 모호한 이월 오류 및 동기화 문제로 실패하게 되는 코드가 생성됩니다.
그렇게 하지 마십시오. 

### 범위

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [이번 장](#범위)

#### 기본적으로 전역 클래스를 사용하고 적절한 경우에만 로컬 클래스를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [범위](#범위) > [이번 장](#기본적으로-전역-클래스를-사용하고-적절한-경우에만-로컬-클래스를-사용하라)

기본적으로 전역 클래스로 작업합니다.
적절한 경우에만 로컬 클래스를 사용하십시오.

> 전역 클래스는 데이터 사전에 표시되는 클래스입니다.
> 로컬 클래스는 다른 개발 개체의 포함 내에 있으며 이 다른 개체에서만 볼 수 있습니다.

로컬 클래스는 아래와 같은 상황에서 쓰입니다.

- 매우 구체적인 개인 데이터 구조의 경우, 예를 들어 전역 클래스의 데이터에 대한 이터레이터는 여기에서만 필요합니다.
  
- 복잡한 private 알고리즘을 추출하기 위한 경우, 예를 들어 클래스 코드의 나머지 부분에서 특수한 목적의 다중 메서드 정렬 집계 알고리즘을 분리해야할 경우 필요합니다.
  
- 전역 클래스의 특정 부분을 mocking 하는 경우, 예를 들어 단위 테스트에서 테스트 이중으로 대체될 수 있는 별도의 로컬 클래스에 대한 모든 데이터베이스 엑세스를 추출합니다.

로컬 클래스는 다른 곳에서 사용할 수 없기 때문에 재사용을 방해합니다.
추출하기는 쉽지만 사람들은 일반적으로 찾지 못하고 원하지도 않는 코드들의 중복이 발생합니다.
매우 긴 로컬 클래스의 오리엔테이션, 탐색 및 디버깅은 지루하고 성가신 일입니다.
시스템에서 Lock을 걸게 되면, 유저들은 로컬의 다른 부분에서 동시에 작업을 할 수가 없습니다(별도의 전역 클래스라면 가능하긴 합니다).

아래와 같은 경우에는 로컬 클래스 사용을 재고하십시오.

- 로컬 클래스가 수십 개의 클래스와 수천 줄의 코드에 걸쳐 있는 경우
- 글로벌 클래스를 다른 클래스를 포함하는 "패키지"로 생각하는 경우
- 글로벌 클래스가 선언만 되고 사용되지 않는 경우
- 별도의 로컬 포함 전체에서 반복되는 중복 코드를 찾은 경우
- 개발자가 작업하면서 서로를 잠그기 시작하고 병렬로 작업할 수 없게 되는 경우
- 팀이 서로의 로컬 하위 트리를 이해하지 못하기 때문에 백로그 추정치가 하늘 높이 치솟는 경우

#### 상속을 고려하지 않았다면 FINAL을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [범위](#범위) > [이번 장](#상속을-고려하지-않았다면-FINAL을-사용하라)

상속을 위해 명시적으로 설계되지 않은 클래스를 `FINAL` 로 만듭니다.

클래스를 설계할 때, 첫 번째 선택은 [상속 대신에 구성을 사용하라](#상속-대신에-구성을-사용하라) 이어야 합니다.
상속을 가능하게 하는 것은 가볍게 할 일이 아닌데 이는  Protected 대 Private, [리스코프 대체원칙](https://en.wikipedia.org/wiki/Liskov_substitution_principle), 등)에 대해 고민해야 하고 디자인 내부도 많이 동결해야 하는 것들까지 신경써야 한다는 것을 의미합니다.
클래스 설계에서 이러한 것들을 고려하지 않았다면, 클래스를 `FINAL`로 만들어 우발적 상속을 방지해야 합니다.

물론 상속을 위한 디자인 패턴 [복합](https://en.wikipedia.org/wiki/Composite_pattern) 과 같은 몇 가지 좋은 응용 프로그램이 _있습니다_.
비즈니스 추가 기능은 또한 하위 클래스를 허용하는데, 이는 개발자들이 대부분의 원본 코드를 재사용할 수 있도록 도움을 줍니다.
그러나 이러한 모든 경우에는 처음부터 설계에 의해 기본 제공되는 상속이 있습니다.

[인터페이스를 구현](#공용-인스턴스-메서드는-항상-인터페이스의-일부여야-한다)하지 않는 깨끗하지 않은 클래스는 개발자가 단위 테스트에서 mocking 할 수 있도록 `FINAL`이 아닌 상태로 두어야 합니다.

#### 기본적으로 Private를 사용하고 필요한 경우에만 Protected를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [범위](#범위) > [이번 장](#기본적으로-Private를-사용하고-필요한-경우에만-Protected를-사용하라)

기본적으로 속성, 메서드 및 기타 클래스 멤버를 `PRIVATE`로 만드십시오.

그것들을 재정의하는 하위 클래스를 활성화하려는 경우에 한해 `PROTECTED`로 만드십시오.

클래스의 내부는 알 필요가 있는 경우에만 다른 사용자에게 제공되어야 합니다.
여기에는 외부 호출자뿐만 아니라 하위 클래스도 포함됩니다.
정보를 과도하게 사용하도록 하면, 예상치 못한 재정의로 인해 미묘한 오류가 발생하고 리팩토링을 방해할 수 있습니다. 이는 외부접근자가 계속적으로 유동적이어야 하는 변수들을 그대로 잡고 있기 때문입니다.

#### Getter 메서드 대신에 PUBLIC과 READ-ONLY를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [범위](#범위) > [이번 장](#Getter-메서드-대신에-PUBLIC과-READ-ONLY를-사용하라)

불변(immutable)은 생성 후 절대 변경되지 않는 객체입니다.
이러한 종류의 객체에는 getter 메서드 대신 PUBLIC & READ-ONLY 속성을 사용하는 것이 좋습니다.

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

아래와 같이 사용하는 것을  지양하십시오.

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

> **Caution**: 값이 **변경되는** 개체의 경우 PUBLIC & READ-ONLY 속성을 사용하지 마십시오. 그렇지 않으면 이 속성은 값이 다른 코드에 필요한지 그렇지 않은지 여부에 관계없이 항상 최신 상태로 유지되어야 합니다.

#### READ-ONLY를 남용하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [범위](#범위) > [이번 장](#READ-ONLY를-남용하지-마라)

최근의 많은 프로그래밍 언어, 특히 Java는 우발적인 부작용을 방지하기 위해 적절한 경우 클래스 멤버를 읽기 전용으로 만들 것을 권장합니다.

ABAP는 데이터 선언에 `READ-ONLY` 추가 기능을 _제공하지만_, 드물게 사용하는 것을 권장합니다.

첫째, `PUBLIC SECTION` 에서만 추가가 가능하여 적용 가능성이 크게 줄어듭니다.
protected 또는 private 멤버나 메서드의 로컬 변수에 추가할 수 없습니다.

둘째, 추가적인 기능은 개발자들이 다른 프로그래밍 언어에서 기대할 수 있는 것과 미묘하게 다릅니다:
`READ-ONLY` 데이터는 여전히 클래스 자체 및 동료&하위 클래스 내의 모든 메서드에서 자유롭게 수정할 수 있습니다.
이것은 다른 언어에서 볼 수 있는 더 널리 퍼진 '한번-입력하면-절대-수정할수없다' 라는 동작과 모순됩니다.
그 차이는 정말 안좋은 결과를 초래할 수도 있습니다.

> 오해를 피하기 위해: 우발적인 수정으로부터 변수를 보호하는 것은 좋은 습관입니다.
> 적절한 설명이 있다면 ABAP에도 적용하는 것이 좋습니다.

### 생성자

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [이번 장](#생성자)

#### CREATE OBJECT 대신에 NEW를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [생성자](#생성자) > [이번 장](#CREATE-OBJECT-대신에-NEW를-사용하라)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

아래와 같이 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

물론 동적 유형이 필요한 경우에는 제외합니다.

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### 전역 클래스가 CREATE private라면 생성자를 public으로 놔두어라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [생성자](#생성자) > [이번 장](#전역-클래스가-CREATE-private라면-생성자를-public으로-놔두어라)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

우리는 이것이 모순된다는 데 동의합니다.
그러나 [ABAP 도움말의 _인스턴스 생성자_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm) 문서에 따르면, 올바른 컴파일 및 구문 유효성 검사를 보장하려면 `PUBLIC SECTION`에 `CONSTRUCTOR`를 지정해야 합니다.

이것은 전역 클래스에만 적용됩니다.
로컬 클래스에서는 생성자를 비공개로 설정해야 합니다.

#### 선택적 파라미터보다 여러 정적인 생성 메서드를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [생성자](#생성자) > [이번 장](#선택적-파라미터보다-여러-정적인-생성-메서드를-사용하라)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP는 [오버로딩](https://en.wikipedia.org/wiki/Function_overloading)을 지원하지 않습니다.
원하는 의미를 얻기 위해 선택적 파라미터가 아닌 이름을 바꾸는 것을 활용하십시오.

```ABAP
" 잘못된 패턴
METHODS constructor
  IMPORTING
    data       TYPE any OPTIONAL
    name       TYPE any OPTIONAL
    object_ref TYPE REF TO object OPTIONAL
    data_ref   TYPE REF TO data OPTIONAL
  [...]
```

일반 지침인 [옵션 파라미터를 추가하는 대신 메서드를 분할하라](#옵션-파라미터를-추가하는-대신-메서드를-분할하라) 에서는 이에 대한 이유를 설명합니다.

[Builder 디자인 패턴](https://en.wikipedia.org/wiki/Builder_pattern)을 사용하여 복잡한 구성을 다단계 구성으로 해결하는 것이 좋습니다.

#### 여러 생성 메서드를 만들 때는 서술적인 이름을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [생성자](#생성자) > [이번 장](#여러-생성-메서드를-만들-때는-서술적인-이름을-사용하라)

Good words to start creation methods are `new_`, `create_`, and `construct_`.  메서드 생성을 시작하기에 좋은 단어는 `new`, `create`, `construct`입니다.
사람들은 직관적으로 그것들을 오브젝트의 구성에 연결합니다.
또한 `new_from_template`, `create_as_copy` 또는 `create_by_name`과 같은 동사구에도 잘 어울립니다.

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

아래와 같이 무의미하게 메서드 이름을 짓지 마십시오.

```ABAP
" 잘못된 패턴
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### 여러 인스턴스가 있는 곳에서만 싱글톤을 만드는 것은 말이 되지 않는다

> [Clean ABAP](#clean-abap) > [내용](#내용) > [클래스](#클래스) > [생성자](#생성자) > [이번 장](#여러-인스턴스가-있는-곳에서만-싱글톤을-만드는-것은-말이-되지-않는다)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

객체 지향 설계에서 두 번째 인스턴스를 갖는 것이 의미가 없다고 말하는 싱글톤 패턴을 적용하십시오.
이를 사용하여 모든 개발자가 동일한 상태 및 동일한 데이터로 동일한 작업을 수행하도록 합니다.

습관적으로 싱글톤 패턴을 사용하지 마십시오.
남용이 가장 많이 되고 잘못 적용된 패턴이며 예기치 않은 교차 효과를 생성하고 불필요하게 테스트를 복잡하게 만듭니다.
단일 오브젝트에 대한 설계가 딱히 정해진 게 없을때에는 이와 관련된 결정을 개발자에게 맡기십시오. 예를 들어, 개발자는 팩토리와 같은 생성자 외부의 수단을 통해 동일한 결과를 낼 수 있습니다.

## 메서드

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#메서드)

이러한 규칙은 클래스 및 함수 모듈의 메서드에 적용됩니다.

### 호출

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#호출)

#### 절차적인 콜보다 펑선콜을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [호출](#호출) > [이번 장](#절차적인-콜보다-펑선콜을-사용하라)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

동적 입력이 펑션콜을 금지하는 경우 절차 스타일을 사용합니다.

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

아래의 많은 세부 규칙은 이 조언의 보다 구체적인 변형일 뿐입니다.

#### RECEIVING을 생략하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [호출](#호출) > [이번 장](#RECEIVING을-생략하라)

```ABAP
DATA(sum) = aggregate_values( values ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### 불필요한 EXPORTING 구문을 생략하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [호출](#호출) > [이번 장](#불필요한-EXPORTING-구문을-생략하라)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### 단일 파라미터 콜에서는 파라미터명을 생략하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [호출](#호출) > [이번 장](#단일-파라미터-콜에서는-파라미터명을-생략하라)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
DATA(unique_list) = remove_duplicates( list = list ).
```

그러나 메서드 이름만으로는 충분히 명확하지 않고 매개변수 이름을 반복하면 더 잘 이해할 수 있는 경우가 있습니다:

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### 인스턴스 메서드를 호출할 때는 자기 참조를 생략하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [호출](#호출) > [이번 장](#인스턴스-메서드를-호출할-때는-자기-참조를-생략하라)

자체 참조 `me->`는 시스템에 의해 암시적으로 설정되므로 인스턴스 메서드를 호출할 때 생략 가능합니다.

```ABAP
DATA(sum) = aggregate_values( values ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
DATA(sum) = me->aggregate_values( values ).
```

### 객체 지향 메서드

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#객체-지향-메서드)

#### 정적 메서드보다 인스턴스를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [객체 지향 메서드](#객체-지향-메서드) > [이번 장](#정적-메서드보다-인스턴스를-사용하라)

메서드는 기본적으로 인스턴스 멤버여야 합니다.
인스턴스 메서드는 클래스의 "객체 후드"를 더 잘 반영합니다.
그들은 단위 테스트 시에 보다 더 쉽게 mocking 됩니다.

```ABAP
METHODS publish.
```

메서드는 정적 생성 메서드와 같은 예외적인 경우에만 정적이어야 합니다.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### 공용 인스턴스 메서드는 항상 인터페이스의 일부여야 한다

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [객체 지향 메서드](#객체-지향-메서드) > [이번 장](#공용-인스턴스-메서드는-항상-인터페이스의-일부여야-한다)

공용 인스턴스 메서드는 항상 인터페이스의 일부여야 합니다.
이것은 의존성을 분리하고 단위 테스트에서 그것들이 mocking 되는 것을 단순화합니다.

```ABAP
METHOD /clean/blog_post~publish.
```

깔끔한 객체 지향에서는 인터페이스 없이 메서드를 public으로 선언하는 것은 의미가 없습니다. 열거형 클래스와 같은 몇 가지 예외적인 경우에는 대체 구현이 없고 테스트 케이스에서 mocking 되지 않습니다.

> [Interfaces vs. abstract classes](sub-sections/InterfacesVsAbstractClasses.md)
에서는 왜 이것들이 상속된 메서드를 덮어쓴 클래스에도 적용되는지를 설명합니다.

### 파라미터 개수

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#파라미터-개수)

#### IMPORTING 파라미터를 3개 이하로 조정하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 개수](#파라미터-개수) > [이번 장](#IMPORTING-파라미터를-3개-이하로-조정하라)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

위 경우가 아래의 경우보다 더욱 깔끔합니다.

```ABAP
" 잘못된 패턴
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

입력 매개변수가 너무 많으면 수많은 조합들을 처리해야 하기 때문에 메서드의 복잡성이 기하급수적으로 증가합니다.
매개변수가 많다는 것은 메서드가 두 가지 이상의 작업을 수행해버릴 수도 있음을 나타내는 지표입니다.

매개변수를 구조화 및 객체화를 통해 의미 있는 집합으로 결합하여 매개변수 수를 줄일 수 있습니다.

#### 옵션 파라미터를 추가하는 대신 메서드를 분할하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 개수](#파라미터-개수) > [이번 장](#옵션-파라미터를-추가하는-대신-메서드를-분할하라)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

ABAP이 [오버로딩](https://en.wikipedia.org/wiki/Function_overloading)을 지원하지 않기 때문에 원하는 의미를 얻기 위해서는 메서드를 분리해야 합니다.

```ABAP
" 잘못된 패턴
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

선택적 매개변수는 호출자를 혼란스럽게 합니다:

- 어떤 것이 정말로 필요한 것인지?
- 어떤 조합이 유효한지?
- 어느 것이 서로를 배제하는지?

use case에 대한 특정 매개변수가 있는 여러 메서드는 어떤 매개변수 조합이 유효하고 예상되는지에 대한 명확한 지침을 제공하여 이러한 혼란을 방지합니다.

#### 우선적 파라미터를 남용하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 개수](#파라미터-개수) > [이번 장](#우선적-파라미터를-남용하지-마라)

`우선적 파라미터`를 추가하면 실제로 어떤 매개변수가 제공되는지 확인하기 어려워 코드를 이해하기 어려워집니다.
매개변수, 특히 선택적 매개변수의 수를 최소화하면 `우선적 파라미터`의 필요성이 자동으로 줄어듭니다.

#### RETURN이나 EXPORT나 CHANGE 중에 하나만 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 개수](#파라미터-개수) > [이번 장](#RETURN이나-EXPORT나-CHANGE-중에-하나만-사용하라)

좋은 메서드는 _하나의 일_ 을 수행하며, 이는 정확히 한 가지를 리턴하는 메서드에도 반영되어야 합니다.
메서드의 출력 파라미터가 논리적 개체를 형성하지 _않으면_  메서드가 한 가지 이상을 수행하게 되므로 이를 분할해야 합니다.

출력이 여러 항목으로 구성된 논리적 개체인 경우가 있습니다.
이것들은 구조체나 객체를 반환함으로써 가장 쉽게 표현됩니다:

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

아래와 같이 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  EXPORTING
    result            TYPE result_type
    failed_keys       TYPE /bobf/t_frw_key
    messages          TYPE /bobf/t_frw_message.
```

특히 여러 EXPOTING 파라미터들을 비교해보면, 이들은 사람들이 펑션콜 스타일을 사용할 수 있게 하는데 이는 어떤 파마리터들이 `제공되는 것인지`에 대해 생각할 필요가 없으며 중요한 `에러 처리`정보를 검색하는 것을 실수로 잊어버리는 것을 방지합니다.

여러 선택적 출력 파라미터 대신 의미 있는 호출 패턴에 따라 메서드를 분할하는 것을 고려하십시오:

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

### 파라미터 유형

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#파라미터-유형)

#### EXPORTING보다 RETURNING을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 유형](#파라미터-유형) > [이번 장](#EXPORTING보다-RETURNING을-사용하라)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
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

`RETURNING`은 호출을 더 짧게 만들 뿐만 아니라 메서드 체인을 허용하고 [입력과 출력이 동시에 이루어지는 것](#입력과-출력이-동시에-이루어지지-않는지-신경-써라)을 방지합니다.

#### 큰 테이블을 다룰 때는 RETURNING을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 유형](#파라미터-유형) > [이번 장](#큰-테이블을-다룰-때는-RETURNING을-사용하라)

따라서 우리는 아래와 같이 사용하는 것을 권장합니다.

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

개별 사례에 대한 실제 증거(= 잘못된 성능 측정)가 있는 경우에만 더 복잡한 절차 스타일에 의존해야 합니다.

```ABAP
" 잘못된 패턴
METHODS get_large_table
  EXPORTING
    result TYPE /dirty/some_table_type.

METHOD get_large_table.
  result = me->large_table.
ENDMETHOD.

get_large_table( IMPORTING result = DATA(my_table) ).
```

> 이 섹션은 ABAP 프로그래밍 지침 및 코드 인스펙터 체크와 모순되며, 둘 다 성능 결함을 피하기 위해서는 큰 테이블이 Export 되어야 한다고 제안합니다.
> 우리는 지속적으로 성능 및 메모리 부족을 재현하지 못했고 일반적으로 RETURNING 성능을 향상시키는 커널 최적화에 대한 알림을 받았습니다.

#### RETURNING이나 EXPORTING이나 CHANGING에 하나를 사용하고 혼합하여 사용하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 유형](#파라미터-유형) > [이번 장](#RETURNING이나-EXPORTING이나-CHANGING에-하나를-사용하고-혼합하여-사용하지-마라)

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

아래와 같이 혼합하여 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
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

다른 종류의 출력 파라미터는 메서드가 한 가지 이상을 수행한다는 것을 의미합니다.
이를 개발자를 혼란스럽게 만들고 메서드 호출을 불필요하게 복잡하게 만듭니다.

이 규칙에 대한 허용 가능한 예외는 출력을 빌드하는 동안 입력을 사용하는 빌더일 수 있습니다:

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

그러나 입력을 객관화하면 그마저도 더 깔끔해질 수 있습니다:

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### CHANGING을 분별하여 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 유형](#파라미터-유형) > [이번 장](#CHANGING을-분별하여-사용하라)

 `CHANGING`은 이미 채워진 기존 로컬 변수가 일부 위치에서만 업데이트되는 경우에 한해 사용해야 합니다:

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

 호출자가 `CHANGING` 파라미터를 제공하기 위해 불필요한 지역 변수를 도입하도록 강요하지 마십시오.
이전에 비어 있던 변수를 처음에 채우기 위해 `CHANGING` 매개변수를 사용하지 마십시오.

#### Boolean 파라미터를 사용하는 대신에 메서드를 분할하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 유형](#파라미터-유형) > [이번 장](#Boolean-파라미터를-사용하는-대신에-메서드를-분할하라)

Boolean 입력 파라미터는 종종 메서드가 하나가 아닌 _둘_ 을 수행한다는 지표입니다.

```ABAP
" 잘못된 패턴
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

또한, 싱글 Boolean 파라미터를 사용한 메서드 호출은 파라미터의 의미를 애매하게 하는 경향이 있습니다.

```ABAP
" 잘못된 패턴
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

메서드를 분할하면 메서드의 코드를 단순화하고 각자의 의도를 더 잘 설명할 수 있습니다.

```ABAP
update_without_saving( ).
update_and_save( ).
```

 일반적인 인식은 Boolean 변수에 대한 setter가 괜찮다고 제안합니다:

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> 아래에서 더 많은 정보를 얻으세요.
> [1](http://www.beyondcode.org/articles/booleanVariables.html)
> [2](https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/)
> [3](http://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### 파라미터명

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#파라미터명)

#### RETURNING-파라미터가-RESULT를-호출하도록-하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터명](#파라미터명) > [이번 장](#RETURNING-파라미터가-RESULT를-호출하도록-하라)

좋은 메서드 이름은 일반적으로 `RETURNING` 매개변수가 자체 이름을 필요로 하지 않을 정도로 좋습니다.
그 이름은 메서드 이름을 항상 앵무새 마냥 똑같은 것들로 명명하거나 명백한 것을 반복하는 것 이상을 수행하지 않습니다.

멤버 이름을 반복하면 불필요한 `me->`를 추가하여 해결해야 하는 충돌이 발생할 수도 있습니다.

```ABAP
" 잘못된 패턴
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

이러한 경우 파라미터수 `RESULT`를 호출하거나 헝가리안 표기법을 선호하는 경우 `RV_RESULT`와 같은 이름을 호출하면 됩니다.

예를 들어 메서드 연결을 위해 'me'를 반환하는 메서드 또는 생성된 엔터티를 반환하지 않고 키만 반환하는 메서드에서와 같이 파라미터의 의미가 명확하지 않은 경우 매개변수의 이름을 지정합니다.

### 파라미터 초기화

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#파라미터-초기화)

#### EXPORTING 참조 파라미터를 초기화하거나 덮어쓰기하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 초기화](#파라미터-초기화) > [이번 장](#EXPORTING-참조-파라미터를-초기화하거나-덮어쓰기하라)

참조 매개변수는 미리 채워질 수 있는 기존 메모리 영역을 참조합니다.
신뢰할 수 있는 데이터를 제공하기 위해 지우거나 덮어씁니다:

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

> 코드 인스펙터와 체크맨은 절대 쓰지 않는 `EXPORTING` 변수를 지적합니다.
이 정적 검사를 사용하여 애매한 오류 코드들을 방지하십시오.

##### 입력과 출력이 동시에 이루어지지 않는지 신경 써라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 초기화](#파라미터-초기화) > [이번 장](#입력과-출력이-동시에-이루어지지-않는지-신경-써라)

일반적으로 형식 및 데이터 선언 후 메서드에서 가장 먼저 파라미터를 지우는 것이 좋습니다.
이렇게 하면 명령문이 쉽게 발견되고 아직 포함된 값이 이후 명령문에서 실수로 사용되는 것을 방지할 수 있습니다.

그러나 일부 매개변수 구성은 입력 및 출력과 동일한 변수를 사용할 수 있습니다.
이 경우 초기 `CLEAR`는 사용하기 전에 입력 값을 삭제하여 잘못된 결과를 야기합니다.

```ABAP
" 잘못된 패턴
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

`EXPORTING`을 `RETURNING`으로 대체하여 이러한 메서드를 재설계하는 것을 고려하십시오.
또한 단일 결과 계산 문에서 `EXPORTING` 파라미터를 덮어쓰는 것을 고려하십시오.
둘 다 맞지 않으면 `CLEAR` 구문을  소스 코드 뒤쪽에 넣어서 사용하십시오.

#### Value 파라미터를 초기화하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [파라미터 초기화](#파라미터-초기화) > [이번 장](#Value-파라미터를-초기화하지-마라)

`VALUE`에 의해 작동하는 파라미터는 정의에 따라 비어 있는 별도의 새 메모리 영역으로 전달됩니다.
다시 지우지 마세요:

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

`RETURNING` 매개변수는 항상 `VALUE` 매개변수이므로 클리어 처리할 필요가 없습니다:

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### 메서드 바디

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#메서드-바디)

#### 하나의 메서드는 한 가지만을 수행하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [메서드 바디](#메서드-바디) > [이번 장](#하나의-메서드는-한-가지만을-수행하라)

메서드는 한 가지만 수행해야 합니다.
가능한 최선의 방법으로 해야 합니다.

메서드는 다음과 같은 경우 한 가지 작업을 수행할 가능성이 높습니다.

- [입력 매개변수가 적은 경우](#IMPORTING-파라미터를-3개-이하로-조정하라)
- [Boolean 파라미터를 포함하지 않는 경우](#Boolean-파라미터를-사용하는-대신에-메서드를-분할하라)
- [정확히 하나의 출력 파라미터가 있는 경우](#RETURN이나-EXPORT나-CHANGE-중에-하나만-사용하라)
- [메서드가 작은 경우](#메서드를-작게-유지하라)
- [한 단계의 추상화 레벨만을 가지는 경우](#한-단계의-추상화-레벨만을-가져라)
- [하나의 예외 처리만을 가지는 경우](#하나의-유형만으로-예외-처리하라)
- 의미 있는 다른 방법을 추출할 수 없는 경우
- 문장을 섹션으로 의미 있게 그룹화할 수 없는 경우

#### HAPPY-PATH 혹은 에러 처리 둘 중 하나에 집중하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [메서드 바디](#메서드-바디) > [이번 장](#HAPPY-PATH-혹은-에러-처리-둘-중-하나에-집중하라)

[하나의 메서드는 한 가지만을 수행하라](#하나의-메서드는-한-가지만을-수행하라) 의 심화버전으로써, 하나의 메서드는 그들이 설계한 happy-path 를 따라가서나 예외처리를 해야하지만, 아마 둘 다에 해당하지 않을지도 모릅니다.

```ABAP
" 잘못된 패턴
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

위 소스는 아래와 같이 분리가 가능합니다.

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

아니면, 아래와 같이 유효성 검사를 강조할 수도 있습니다.

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

#### 한 단계의 추상화 레벨만을 가져라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [메서드 바디](#메서드-바디) > [이번 장](#한-단계의-추상화-레벨만을-가져라)

메서드 구문은 각각 한 단계의 추상화 레벨을 가져야합니다.
따라서 모두 동일한 추상화 수준에 있어야 합니다.

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

아래와 같이 하위 레벨(`trim`, `to_upper`)과 상위 레벨(`publish`)을 혼용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

올바른 추상화 수준이 무엇인지 알아내는 신뢰할 수 있는 방법은 다음과 같습니다:
메서드 작성자가 코드를 보지 않고 메서드가 하는 일을 짧은 단어로 설명하게 하십시오.
그가 번호를 매긴 글머리 기호는 메서드가 호출해야 하는 하위 메서드 또는 실행해야 하는 명령문입니다.

#### 메서드를 작게 유지하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [메서드 바디](#메서드-바디) > [이번 장](#메서드를-작게-유지하라)

메서드는 20개 미만의 문을 포함해야 하며 3~5개 정도가 최적입니다.

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

다음 `DATA` 선언만으로도 주변 메서드가 한 가지 이상을 수행한다는 것을 알 수 있습니다:

```ABAP
" 잘못된 패턴
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

물론 더 큰 메서드를 더 줄이는 것이 이치에 맞지 않는 경우도 있습니다.
메서드가 [하나의-메서드는-한-가지만을-수행하라](#하나의-메서드는-한-가지만을-수행하라)에 계속 해당되는 한 이것은 완벽하게 괜찮습니다.

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

그러나 자세한 코드일수록 적합한 패턴들을 숨기는지의 여부를 확인하는 것은 여전히 의미가 있습니다.

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> 방법을 아주 작게 자르면 메서드 호출 수가 증가하기 때문에 성능에 나쁜 영향을 줄 수 있습니다.
> [_수행속도를 신경 써라_](#수행속도를-신경-써라) 섹션에서는 클린 코드와 성능의 균형을 맞추는 방법에 대한 지침을 제공합니다.

### 제어 흐름

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [이번 장](#제어-흐름)

#### 검증을 빠르게 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [제어 흐름](#제어-흐름) > [이번 장](#검증을-빠르게-하라)

검증과 실패는 최대한 빨라야 합니다.

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

검증을 늦게 할 수록 발견하기도 어렵고 이해하기가 더 어려우며 거기에 도달하기 위해 이미 많은 자원을 낭비했을 수도 있습니다.

```ABAP
" 잘못된 패턴
METHOD do_something.
  DATA(massive_object) = build_expensive_object_from( input ).
  IF massive_object IS NOT BOUND. " happens if input is initial
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

#### CHECK vs RETURN

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [제어 흐름](#제어-흐름) > [이번 장](#check-vs-return)

현재는 입력이 기대에 미치지 못하는 경우 메서드를 종료하기 위해 'CHECK' 또는 'RETURN'을 사용해야 하는지에 대한 합의가 없습니다.

`CHECK`는 확실히 더 짧은 구문을 제공합니다.

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

check 구문은 조건이 실패하면 어떻게 되는지 나타내지 않으므로 사람들이 `RETURN` 형식을 오히려 더 잘 이해할 수 있습니다.

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD.
```

아래와 같이 유효성 검사를 반대로 하고 single-return 제어 흐름을 채택하면 질문을 완전히 피할 수 있습니다.

```ABAP
METHOD read_customizing.
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD.
```

어떤 경우든 아무 것도 반환하지 않는 것이 실제로 적절한 행동인지 고려하십시오.
메서드는 채워진 리턴 파라미터 또는 예외 처리 등의 의미 있는 결과를 제공해야 합니다.
아무것도 반환하지 않는 것은 많은 경우 `null`을 반환하는 것과 유사하므로 피해야 합니다.

> [ABAP 프로그래밍 지침의 _종료 절차_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)  섹션에 해당하는 경우에는 `CHECK`를 사용하는 것이 좋습니다.
> 커뮤니티 토론에 따르면 진술이 너무 명확하지 않아 많은 사람들이 프로그램의 동작을 이해하지 못할 것입니다.

#### 일부 위치에서는 CHECK 사용을 지양하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [메서드](#메서드) > [제어 흐름](#제어-흐름) > [이번 장](#일부-위치에서는-CHECK-사용을-지양하라)

메서드의 초기화 섹션 외부에서 `CHECK`를 사용하지 마십시오.
명령문은 위치에 따라 다르게 작동하며 불명확하고 예상치 못한 결과를 초래할 수 있습니다.

예를 들어,  [`LOOP`의 `CHECK`는 현재 반복을 종료하고 다음 반복으로 진행합니다](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm);
그런데 개발자들은 그것을 메서드를 종료하거나 루프를 종료할 것으로 예상할 수 있습니다.
`CONTINUE`는 루프에서만 사용할 수 있으므로 `CONTINUE`와 함께 `IF` 문을 사용하는 것이 좋습니다.

> [ABAP 프로그래밍 지침의_종료 절차_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm) 섹션을 기반으로 하였습니다.
> 이는 [Loop 구문에서 `CHECK`에 대한 키워드 참조](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm) 의 내용과 모순되긴 합니다.

## 에러 처리

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#에러-처리)

### 메시지

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [이번 장](#메시지)

#### 메시지를 찾기 쉽게 만들어라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [메시지](#메시지) > [이번 장](#메시지를-찾기-쉽게-만들어라)

Tcode SE91에서 사용 위치 검색을 통해 메시지를 쉽게 찾을 수 있도록 하려면 다음 패턴을 사용하십시오:

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

변수 `message`가 필요하지 않은 경우 `##NEEDED` pragma를 추가합니다.

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

아래와 같이 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

아래는 잘못된 패턴들입니다.
- 연결할 수 없는 코드가 포함되어 있는 경우
- 결코 참이 될 수 없는 조건을 테스트하는 경우

### 리턴 코드

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [이번 장](#리턴-코드)

#### 리턴 코드보다 예외 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [리턴 코드](#리턴-코드) > [이번 장](#리턴-코드보다-예외-구문을-사용하라)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

아래와 같이 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

예외처리는 Return 코드에 비해 여러 가지 이점이 있습니다:

- 예외처리는 당신의 메서드를 깔끔하게 합니다:
  메서드의 결과를 `RETURNING` 파라미터로 반환하고 여전히 예외를 던질 수 있습니다.
  Return 코드는 오류 처리를 위한 추가 파라미터로 당신의 메서드를 지저분하게 만듭니다.
- 즉시 응답할 필요가 없습니다. 그는 단순히 코드의 Happy Path만을 기록하고, `CATCH` 구문을 활용하여 메서드의 맨끝이나 완전히 외부에서 예외처리를 할 수 있습니다.
  
- 예외처리는 속성 및 메서드를 통해 오류에 대한 세부 정보를 제공할 수 있습니다.
  반환 코드를 사용하려면 로그 반환과 같이 스스로 다른 솔루션을 고안해야 합니다.
- 예외처리 환경은 호출자에게 예외를 처리하도록 구문 오류를 상기시킵니다.
  반환 코드는 아무도 모르게 실수로 무시될 수도 있습니다.

#### 오류를 놓치지 않도록 만들어라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [리턴 코드](#리턴-코드) > [이번 장](#오류를-놓치지-않도록-만들어라)

예를 들어 사용자가 제어할 수 없는 펑션 및 이전 코드를 호출하는 과정에서 Return 코드를 사용해야 하는 경우 오류가 발생하지 않도록 하십시오.

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

### 예외 처리

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [이번 장](#예외-처리)

#### 일반적인 케이스가 아닐 때의 에러는 예외 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리](#예외 처리) > [이번 장](#일반적인-케이스가-아닐-때의-에러는-예외-구문을-사용하라)

```ABAP
" 잘못된 패턴
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

어떤 것이 규칙적이고 유효한 경우라면 일반적인 결과 파라미터로 처리해야 합니다.

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

예상하지 못한 경우와 오류 상황을 반영하는 경우에는 예외처리를 해야 합니다.

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

예외를 잘못 사용하면 실제로 모든 것이 정상인데도 개발자가 뭔가 잘못되었다고 생각하도록 잘못 안내합니다.
예외는 잘 구성되어야 하고 종종 많은 컨텍스트 정보를 수집해야 하기 때문에 일반 코드보다 훨씬 느립니다.

#### 클래스에 기반한 예외 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리](#예외 처리) > [이번 장](#클래스에-기반한-예외-구문을-사용하라)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

오래된 비클래스 기반 예외는 반환 코드와 동일한 기능을 가지며 더 이상 사용하지 않아야 합니다.

```ABAP
" 잘못된 패턴
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### 예외 처리 구문

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [이번 장](#예외-처리-구문)

#### 슈퍼 클래스를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#슈퍼-클래스를-사용하라)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```

기초 클래스를 직접 서브클래싱하는 대신 애플리케이션의 각 예외 유형에 대해 추상 수퍼 클래스를 만드는 것을 고려하십시오.
_당신의_  모든 예외를 `CATCH` 할 수 있습니다.
특수 텍스트 처리와 같은 모든 예외에 공통 기능을 추가할 수 있습니다.
`ABSTRACT`는 사람들이 이러한 비설명적 오류를 실수로 직접 사용하는 것을 방지합니다.

#### 하나의 유형만으로 예외 처리하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#하나의-유형만으로-예외-처리하라)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

대부분의 경우 여러 유형의 예외 처리를 하는 것은 아무 소용이 없습니다.
호출자는 일반적으로 오류 상황에 관심이 없고 구별할 수 없습니다.
따라서 그는 일반적으로 모든 항목을 같은 방식으로 처리합니다. 
이 경우 왜 그들을 우선적으로 구분해야 합니까?

```ABAP
" 잘못된 패턴
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

다양한 오류 상황을 인식하는 더 나은 솔루션은 하나의 예외 유형을 사용하는 것입니다.
그러나 [호출자가 오류 상황을 구별할 수 있도록 하위 클래스를 사용](#호출자가-오류-상황을-구별할-수-있도록-하위-클래스를-사용하라)에 설명된 대로, 하위 클래스를 추가하는 것은 개별적인 오류 상황에 반응할 수 있게 합니다.

#### 호출자가 오류 상황을 구별할 수 있도록 하위 클래스를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#호출자가-오류-상황을-구별할-수-있도록-하위-클래스를-사용하라)

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

다양한 오류 상황이 있는 경우 대신 오류 코드를 사용하십시오:

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

#### 관리 가능한 예외 처리 구문에 대해 CX_STATIC_CHECK 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#관리-가능한-예외-처리-구문에-대해-CX_STATIC_CHECK-구문을-사용하라)

예외가 발생할 것으로 예상되고 수신자가 합리적으로 처리할 수 있는 경우 `CX_STATIC_CHECK`에서 상속되는 예외를 처리합니다: 실패한 사용자 입력 유효성 검사, 대체가 있는 누락된 리소스 등

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

이 예외 유형은 메서드에 지정되어야 하고 캐치하거나 구문 오류를 방지하기 위해 _꼭_  필요합니다.
이것들은 개발자가 쉽게 볼 수 있으며 예기치 않은 예외에 놀라지 않고 오류 상황에 대처할 수 있도록 합니다.

> 이것들은 [ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenexception_category_guidl.htm) 의 내용입니다.
> 그러나  [로버트 마틴의 _Clean Code_] 과는 모순 되는데, 이는 확인되지 않은 예외를 선호하는 것이 좋다는 내용들입니다. 
> [예외](sub-sections/Exceptions.md) 파일에서 이유를 설명합니다.

#### 복구할 수 없는 상활일 때에는 CX_NO_CHECK 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#복구할-수-없는-상활일-때에는-CX_NO_CHECK-구문을-사용하라)

예외가 너무 심각하여 수신자가 복구할 수 없는 경우 `CX_NO_CHECK`를 사용합니다: 필수 리소스 읽기 실패, 요청된 종속성 해결 실패 등

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK`는 메서드 시그니처에서 _선언할 수 없는데_, 그렇게 개발하면 개발자에게 별로 좋지 못합니다.
복구할 수 없는 상황의 경우 개발자가 어쨌든 이에 대해 유용한 조치를 취할 수 없기 때문에 괜찮습니다.

그러나 개발자가 실제로 이러한 종류의 실패를 인식하고 대응하기를 원하는 경우가 _있을 수 있습니다_.
예를 들어 종속성 관리자는 일반 애플리케이션 코드를 계속할 수 없기 때문에 요청된 인터페이스에 대한 구현을 제공할 수 없는 경우 `CX_NO_CHECK'를 사용해서 예외처리할 수 있습니다.
그러나 작동 여부를 확인하기 위해 모든 종류의 항목을 인스턴스화하려고 시도하는 테스트 보고서가 있을 수 있으며, 이는 단순히 목록의 빨간색 항목으로 실패를 보고합니다.
이러한 서비스의 경우에는 덤프를 강요하는 대신에 예외를 포착하고 무시할 수 있어야 합니다. 

#### 회피할 수 있는 상황에서는 CX_DYNAMIC_CHECK 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#회피할-수-있는-상황에서는-CX_DYNAMIC_CHECK-구문을-사용하라)

`CX_DYNAMIC_CHECK`의 사용 사례는 드물며 일반적으로 다른 예외 유형을 사용하는 것이 좋습니다.
그러나 호출자가 예외가 발생할 수 있는지 여부를 의식적으로 완전히 제어할 수 있는 경우 'CX_STATIC_CHECK'를 대체하는 것으로 이러한 종류의 예외를 고려할 수 있습니다.

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

 예를 들어, 10진 부동 소수점 숫자의 자릿수와 소수 자릿수를 알려주는 클래스 `cl_abap_math`의 `get_db_length_decs` 메서드를 고려하십시오.
이 메서드는 입력 매개변수가 10진수 부동 소수점 숫자를 반영하지 않는 경우 동적 예외 `cx_parameter_invalid_type`을 발생시킵니다.
일반적으로 이 메서드는 완전하면서도 정적으로 형식화된 변수에 대해 호출되므로 개발자는 예외가 발생할 수 있는지 여부를 알 수 있습니다.
이 경우 동적 예외로 인해 호출자가 불필요한 'CATCH' 절을 생략할 수 있습니다.

#### 완전히 복구가 불가능한 상황에서는 DUMP 처리하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#완전히-복구가-불가능한-상황에서는-DUMP-처리하라)

상황이 너무 심각하여 수신기가 복구할 가능성이 거의 없다고 확신하거나 프로그래밍 오류를 분명히 나타내는 경우 예외처리를 하는 대신 덤프하십시오: 메모리 확보 실패, 채워야 하는 테이블에 대한 실패한 인덱스 읽기 등

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

이 동작은 모든 종류의 사용자가 나중에 유용한 작업을 수행하지 못하게 합니다.
확신이 있는 경우에만 사용하십시오.

#### RAISE EXCEPTION TYPE보다 RAISE EXCEPTION NEW를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [예외 처리 구문](#예외-처리-구문) > [이번 장](#RAISE-EXCEPTION-TYPE보다-RAISE-EXCEPTION-NEW를-사용하라)

참고: NW 7.52부터 사용 가능합니다.

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

일반적으로 사용했던 코드들보다 더 짧습니다.

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

그러나 추가 'MESSAGE'를 많이 사용하는 경우 'TYPE' 변수를 고수하고 싶을 수 있습니다:

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception
  MESSAGE e136(messages).
```

### Catch 구문

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [이번 장](#catch-구문)

#### 외부에서 당신의 코드를 침범하지 못하도록 예외 처리하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [에러 처리](#에러-처리) > [Catch 구문](#catch-구문) > [이번 장](#외부에서-당신의-코드를-침범하지-못하도록-예외-처리하라)

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

[데메테르의 법칙](https://en.wikipedia.org/wiki/Law_of_Demeter)은 디커플링을 권장합니다.
다른 구성 요소에서 예외를 전달하면 이 원칙을 위반합니다.
이러한 예외를 포착하고 자신의 예외 유형으로 래핑하여 외부 코드로부터 자신을 독립시키십시오.

```ABAP
" 잘못된 패턴
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## 주석

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#주석)

### 당신의 생각을 주석이 아닌 코드에 표현하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#당신의-생각을-주석이-아닌-코드에-표현하라)

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

아래와 같이 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
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

Clean Code는 코드에 주석을 추가하는 것을 금지하지 _않습니다_.  Clean Code는 당신이 더 나은 수단을 이용하도록 장려하고 그것이 실패할 경우에만 주석을 사용하도록 권장합니다.

> 이 예는 메서드를 너무 작은 단위로 쪼개면 성능이 너무 많이 악화된다고 주장하면서 성능 관점에서 도전을 받았습니다.
> 샘플 측정에 따르면 리팩토링된 코드는 원래 더티 변수보다 2.13배 더 느립니다.
> 깔끔한 변수은 `31-02-2018` 입력을 수정하는 데 9.6마이크로초가 걸리고 더티 변수은 4.5마이크로초밖에 걸리지 않습니다.
> 이것은 메서드가 고성능 응용 프로그램에서 매우 자주 실행될 때 문제가 될 수 있습니다; 하지만 일반 사용자 입력 유효성 검사의 경우 허용되어야 합니다.
> 클린 코드 및 성능 문제를 처리하려면 [수행속도를 신경 써라](#수행속도를-신경-써라) 섹션을 참조하세요.

### 주석으로 핑계를 대지 말고 코드를 변경하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#주석으로-핑계를-대지-말고-코드를-변경하라)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

이름의 진정한 의미나 나쁜 이름을 선택한 이유를 설명하는 대신 이름을 개선하십시오.

```ABAP
" 잘못된 패턴
" checks whether the table input contains entries
DATA(result) = check_table( input ).
```

### 코드를 나눠서 주석하는 대신에 메서드를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#코드를-나눠서-주석하는-대신에-메서드를-사용하라)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

이렇게 하면 코드의 의도, 구조 및 종속성이 훨씬 더 명확해질 뿐만 아니라 섹션 간에 임시 변수가 제대로 지워지지 않을 때 이월 오류도 방지됩니다.

```ABAP
" 잘못된 패턴
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

### 코드에 대한 이유를 주석으로 작성하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#코드에-대한-이유를-주석으로-작성하라)

```ABAP
" can't fail, existence of >= 1 row asserted above
DATA(first_line) = table[ 1 ].
```

아무도 자연어로 코드를 반복할 필요가 없습니다.

```ABAP
" 잘못된 패턴
" select alert root from database by key
SELECT * FROM d_alert_root WHERE key = key.
```

### 설계에 관련된 내용을 주석으로 작성하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#설계에-관련된-내용을-주석으로-작성하지-마라)

```ABAP
" 잘못된 패턴
" This class serves a double purpose. First, it does one thing. Then, it does another thing.
" It does so by executing a lot of code that is distributed over the local helper classes.
" To understand what's going on, let us at first ponder the nature of the universe as such.
" Have a look at this and that to get the details.
```

아무도 그것들을 읽지 않습니다.
다른 개발자들이 당신의 코드를 사용하기 위해 가이드를 읽어야 한다면, 이것은 당신의 코드에 다른 방법으로 해결해야 하는 심각한 디자인 문제가 있다는 표시일 수 있습니다.
일부 코드는 한 줄의 주석을 넘어서는 설명이 _필요_합니다; 
이러한 경우 설계 문서를 연결하는 것을 고려하십시오.

### 별이 아닌 큰 따옴표를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#별이-아닌-큰-따옴표를-사용하라)

인용문 주석은 주석문과 함께 들여쓰기됩니다.

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

애스터리스크 주석은 이상한 곳으로 들여쓰기되는 경향이 있습니다.

(역자주: MD 특성 상 *표시를 할 수가 없어 애스터리스크 대신에 별이라고 표시하였음)

```ABAP
" 잘못된 패턴
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### 코드 바로 위에 주석을 작성하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#코드-바로-위에-주석을-작성하라)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

위 코드가 아래의 코드보다도 훨씬 깔끔합니다.

```ABAP
" 잘못된 패턴
output = calculate_result( input ).
" delegate pattern
```

아래보다도 낫습니다.

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### 주석 처리한 코드는 삭제하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#주석-처리한-코드는-삭제하라)

```ABAP
" 잘못된 패턴
* output = calculate_result( input ).
```

이와 같은 것을 발견하면 삭제하십시오.
애플리케이션이 작동하고 모든 테스트가 정상적으로 동작하기 때문에 위와 같은 코드는 분명히 필요하지 않습니다.
삭제된 코드는 나중에 버전 기록에서 재생할 수 있습니다.
코드 조각을 영구적으로 보존해야 하는 경우 파일이나 `$TMP` 또는 `HOME` 개체에 복사합니다.

### FIXME TODO XXX 그리고 당신의 아이디를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#FIXME-TODO-XXX-그리고-당신의-아이디를-사용하라)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME`는 내부 사고에 대해 너무 작거나 너무 많은 오류를 나타냅니다.
- `TODO`는 가까운(!) 미래에 무언가를 완료하고 싶은 곳입니다.
- `XXX`는 작동하지만 더 좋을 수 있는 코드를 표시합니다.

이러한 주석을 입력할 때 닉네임, 이니셜 또는 사용자를 추가하여 공동 개발자가 당신에게 연락하고 주석이 명확하지 않은 경우 질문할 수 있도록 합니다.

### 메서드 서명이나 끝에 주석을 달지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#메서드-서명이나-끝에-주석을-달지-마라)

메서드 서명 주석은 아무에게도 도움이 되지 않습니다.

```ABAP
" 잘못된 패턴
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CALIBRATION_KPIS=>CALCULATE_KPI
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRATEGY_ID                 TYPE        STRATEGY_ID
* | [--->] THRESHOLD                   TYPE        STRATEGY_THRESHOLD
* | [--->] DETECTION_OBJECT_SCORE      TYPE        T_HIT_RESULT
* | [<---] KPI                         TYPE        T_SIMULATED_KPI
* +--------------------------------------------------------------------------------------</SIGNATURE>
```

수십 년 전 코드를 검사하거나 수십 페이지가 있는 인쇄물로 작업할 때 메서드 서명을 볼 수 없었을 때 이러한 주석이 이해가 되었을 수 있습니다.
그러나 모든 최신 ABAP IDE(SE24, SE80, ADT)는 메서드 서명을 쉽게 보여주므로 이러한 주석은 노이즈에 불과합니다.

> SE24/SE80의 양식 기반 편집기에서 _Signature_ 버튼을 누릅니다.
> ABAP 개발 도구에서 메서드 이름을 표시하고 F2 키를 누르거나 _ABAP 요소 정보_ 보기를 퍼스펙티브에 추가합니다.

마찬가지로, 주석의 끝은 불필요합니다.
이러한 주석은 프로그램과 함수, 내부에 중첩된 IF가 수백 줄의 코드 길이였던 수십 년 전에 도움이 되었을 수 있습니다.
그러나 우리의 현대적인 코딩 스타일은 `ENDIF` 또는 `ENDMETHOD`가 속하는 시작 문장을 쉽게 볼 수 있을 만큼 충분히 짧은 메서드를 생성합니다.

```ABAP
" 잘못된 패턴
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### 메시지 텍스트를 주석으로 재사용하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#메시지-텍스트를-주석으로-재사용하지-마라)

```ABAP
" 잘못된 패턴
" alert category not filled
MESSAGE e003 INTO dummy.
```

메시지는 코드와 독립적으로 변경되며 아무도 모르게 주석을 조정한 것을 기억하지 못할 것입니다.

최신 IDE를 사용하면 메시지 뒤에 있는 텍스트를 쉽게 볼 수 있습니다. 예를 들어 ABAP 개발 도구에서 메시지 ID를 표시하고 Shift+F2를 누릅니다.

더 명확하게 하려면 메시지를 고유한 방법으로 추출하는 것이 좋습니다.

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### 공개 API를 문서화하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#공개-API를-문서화하라)

ABAP 문서를 작성하여 다른 팀이나 애플리케이션의 개발자를 위한 API를 의미하는 공개 API를 문서화하십시오.
내부용으로 ABAP 문서를 작성하지 마십시오.

ABAP Doc은 모든 주석과 동일한 약점을 가지고 있는데 다시 말해 빠르게 구식화되어 오해의 소지가 있습니다.
결과적으로 모든 것에 대해 ABAP 문서를 작성하도록 강요하지 말고 합리적인 경우에만 사용해야 합니다.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 4: Good Comments: Javadocs in Public APIs_와 _챕터 4: Bad Comments: Javadocs in Nonpublic Code_를 참고하십시오.

### 슈도 주석보다 프라그마를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [주석](#주석) > [이번 장](#슈도-주석보다-프라그마를-사용하라)

ATC에서 식별한 관련 없는 경고 및 오류를 억제하려면 슈도 주석보다 프라그마를 선호하십시오. 슈도 주석은 대부분 쓸모없게 되어 프라그마로 대체되었습니다.

```ABAP
" 패턴
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" 잘못된 패턴
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

프로그램 `ABAP_SLIN_PRAGMAS` 또는 테이블 `SLIN_DESC`를 사용하여 더 이상 사용되지 않는 슈도 주석과 이를 대체한 프라그마 간의 매핑을 찾으십시오.

(역자주: [관련 링크](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenpragma.htm) )

## 서식

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#서식)

서식과 관련되 내용들은 [쓰기가-아니라-읽기를-위한-최적화를-하라](#쓰기가-아니라-읽기를-위한-최적화를-하라) 섹션에 있습니다.
ABAP의 Pretty Printer는 이를 다루지 않기 때문에 일부는 이름 길이 등이 변경될 때 명령문의 서식을 다시 지정하기 위해 추가 수동 작업을 생성합니다; 만약 당신이 이를 피하고 싶다면,  [동일한 오브젝트에 한해서는 라인을 정렬하라](#동일한-오브젝트에-한해서는-라인을-정렬하라) 과 같은 규칙은 삭제하는 것이 좋습니다.

### 동일한 양식을 유지하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#동일한-양식을-유지하라)

같은 방식으로 프로젝트의 모든 코드를 포맷팅하십시오
모든 팀 구성원이 동일한 서식 스타일을 사용하도록 합니다.

외부 코드를 편집하는 경우 개인 스타일을 고집하는 대신 해당 프로젝트의 서식 스타일을 따르십시오.

시간이 지남에 따라 형식 지정 규칙을 변경하는 경우 [레거시-코드를-리팩토링하는-방법](#레거시-코드를-리팩토링하는-방법)을 사용하여 코드를 업데이트하세요.

### 쓰기가 아니라 읽기를 위한 최적화를 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#쓰기가-아니라-읽기를-위한-최적화를-하라)

개발자는 코드를 _읽는 데_  대부분의 시간을 보냅니다.
실제로 코드를 _작성하는 데_  보내는 시간은 하루 중 아주 작은 부분에 불과합니다.

결과적으로 쓰기가 아니라 읽기 및 디버깅을 위해 코드 형식을 최적화해야 합니다.

예를 들어, 당신은 아래와 같이 사용하는 것이 더 낫습니다.

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

아래처럼 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### Activate 버튼을 누르기 전에 Pretty Printer를 먼저 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#Activate-버튼을-누르기-전에-Pretty-Printer를-먼저-하라)

오브젝트를 활성화하기 전에 _Pretty Printer_(SE80, SE24 및 ADT에서 Shift+F1)를 적용하십시오.

형식이 지정되지 않은 더 큰 레거시 코드 기반을 수정하는 경우 선택 항목에만 _Pretty Printer_를 적용하여 변경 목록과 전송 종속성을 방지할 수 있습니다.
별도의 Transport Request 나 Note에서 완전한 개발 오브젝트를 Pretty-Printing 하십시오.

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 5: Formatting: Team Rules_ 를 참고하십시오.

### Pretty Printer 설정은 팀에서 정한대로 따라가라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#Pretty-Printer-설정은-팀에서-정한대로-따라가라)

Always use your team settings. 항상 팀 설정을 사용하십시오.
아래 메뉴에서 해당 설정을 할 수 있습니다.
_Menu_ > _Utilities_ > _Settings ..._ > _ABAP Editor_ > _Pretty Printer_.

팀에서 합의한대로 _Indent_ 나 _Convert Uppercase/Lowercase_ > _Uppercase Keyword_ 를 설정하십시오.

> [대소문자 비교](sub-sections/UpperVsLowerCase.md)는 키워드의 대소문자에 대한 명확한 지침을 제공하지 않는 이유를 설명합니다.
> 
>자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 5: Formatting: Team Rules_ 를 참고하십시오.

### 한 줄에는 하나의 구문만 존재하게 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#한-줄에는-하나의-구문만-존재하게-하라)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

어떤 경우에는 이것이 읽을 수 있을 것이다 라고 생각될 수 있지만 잘못된 패턴입니다.

```ABAP
" 잘못된 패턴
DATA do_this TYPE i. do_this = input + 3.
```

### 합리적인 라인별 코드 길이를 준수하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#합리적인-라인별-코드-길이를-준수하라)

한줄당 최대 120자의 줄 길이를 준수하십시오.

사람의 눈은 라인이 너무 옆으로 길게 늘어져 있지 않을수록 텍스트를 더 편안하게 읽습니다. UI 디자이너나 안구 운동 연구원에게 자세한 것들을 문의하세요.
또한 서로 나란히 있는 두 소스를 디버깅하거나 비교할 때 더 좁은 코드에 감사하게 될 것입니다.

이전 버전에서의 길이 제한인 80자 또는 72자는 너무 제한적입니다.
종종 100자가 권장지만 120자가 ABAP에 조금 더 잘 작동하는 걸로 보이는데, 이는 아마도 언어의 일반적인 장황함 때문일 수 있습니다.

> 참고로 ADT에서 인쇄 여백을 120자로 구성할 수 있는데, 그러면 코드 보기에서 수직선으로 시각화됩니다.
> 이는 _Menu_ > _Window_ > _Preferences_ > _General_ > _Editors_ > _Text Editors_ 에서 설정이 가능합니다.

### 불필요한 공백을 줄여라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#불필요한-공백을-줄여라)

```ABAP
DATA(result) = calculate( items ).
```

아래와 같이 불필요한 공백을 추가하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
DATA(result)        =      calculate(    items =   items )   .
```

### 역할을 구분하기 위해서는 한 줄만 추가하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#역할을-구분하기-위해서는-한-줄만-추가하라)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

두 문장이 다른 일을 한다는 것을 강조하기 위해 그럴수도 있지만, 사실 그럴 이유가 없습니다.

```ABAP
" 잘못된 패턴
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

빈 줄 구분을 추가하려는 충동은 귀하의 방법이 [한 가지만](#하나의-메서드는-한-가지만을-수행하라) 수행하지 않는다는 표시일 수 있습니다.

### 굳이 빈 줄을 추가하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#굳이-빈-줄을-추가하지-마라)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

빈 줄로 코드를 분리하는 것이 나쁜 습관이라는 것에는 이유가 없습니다.

```ABAP
" 잘못된 패턴
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

빈 줄은 실제로 여러 줄에 걸쳐 있는 문이 있는 경우에만 의미가 있습니다.

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

### 동일한 오브젝트에 한해서는 라인을 정렬하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#동일한-오브젝트에-한해서는-라인을-정렬하라)

이러한 것들이 어떻게든 함께 속해 있음을 정렬을 통해 강조할 수도 있습니다. (아래는 = 기호 위치가 정렬)

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

아니면 차라리 아래와 같은 방식도 괜찮습니다.

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

그러나 서로 관련이 없는 것은 들쭉날쭉한 상태로 두십시오.

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access = fra_hdbr_access=>s_get_instance( ).
```

> 자세한 내용은 [로버트 마틴의 _Clean Code_] _챕터 5: Formatting: Horizontal Alignment_ 를 참고하십시오.

### 괄호는 라인의 끝에 위치하게 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#괄호는-라인의-끝에-위치하게-하라)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### 단일 파라미터 콜은 한 줄에 작성하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#단일-파라미터-콜은-한-줄에-작성하라)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

아래와 같이 불필요하게 긴 코드를 지양하십시오.

```ABAP
" 잘못된 패턴
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### 호출문 뒤에 파라미터를 위치하게 하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#호출문-뒤에-파라미터를-위치하게-하라)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

이로 인해 줄이 매우 길어지면 파라미더를 그 다음 줄로 내릴 수 있습니다.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### 호출문 아래 파라미터들을 들여쓰기하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#호출문-아래-파라미터들을-들여쓰기하라)

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

파라미터를 다른 곳에 정렬하면 해당 파라미터가 속한 항목을 찾기가 어렵습니다.

```ABAP
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

그러나 이름 길이 변경으로 인해 포맷팅이 깨지는 것을 방지하려면 이 패턴이 가장 좋습니다.

### 파라미터가 여러 개일 때는 라인을 분리하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#파라미터가-여러-개일-때는-라인을-분리하라)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

맞습니다. 이것은 공간을 낭비합니다.
그러나 그렇지 않으면 한 파라미터가 끝나고 다음 파라미터가 시작되는 위치를 찾기가 어렵습니다:

```ABAP
" 잘못된 패턴
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### 파라미터들을 정렬하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#파라미터들을-정렬하라)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

불규칙한 여백으로 인해 파라미터가 끝나는 위치와 값이 시작되는 위치를 확인하기 어렵습니다.

```ABAP
" 잘못된 패턴
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> 변수들의 길이 변경으로 인해 서식이 손상되는 것을 방지하려는 경우 가장 좋은 패턴입니다.

### 코드가 너무 긴 경우에는 새로운 라인으로 호출문을 분리하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#코드가-너무-긴-경우에는-새로운-라인으로-호출문을-분리하라)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### Tab 버튼을 활용해 들여쓰기하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#Tab-버튼을-활용해-들여쓰기하라)

파라미터 키워드는 2칸, 파라미터는 4칸 들여쓰기하십시오:

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

키워드가 없는 경우 파라미터를 4칸 들여쓰기합니다.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

들여쓰기를 하려면 Tab 키를 사용하세요. 필요한 것보다 하나의 공간을 더 추가해도 괜찮습니다.
(위 경우는 왼쪽의 `DATA(sum) =` 부분의 문자 수가 홀수인 경우에 발생합니다.)

### 메서드 호출인 것처럼 들여쓰기하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#메서드-호출인-것처럼-들여쓰기하라)

메서드 호출인 것처럼 VALUE 또는 NEW를 사용하여 인라인 선언을 들여씁니다.

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### Type절은 정렬하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [서식](#서식) > [이번 장](#Type절은-정렬하지-마라)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

변수와 해당 유형은 함께 속하므로 시각적으로 가깝게 그룹화되어야 합니다.
`TYPE` 절을 정렬하면 그로부터 주의를 끌며 변수가 하나의 수직 그룹을 형성하고 해당 유형이 또 다른 수직 그룹을 형성함을 시사합니다.
또한 정렬은 불필요한 편집 오버헤드를 생성하므로 가장 긴 변수 이름의 길이가 변경될 때 모든 들여쓰기를 조정해야 합니다.

```ABAP
" 잘못된 패턴
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

## 테스트

> [Clean ABAP](#clean-abap) > [내용](#내용) > [이번 장](#테스트)

### 테스트 원칙

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#테스트-원칙)

#### 테스트가 가능하도록 코딩하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 원칙](#테스트-원칙) > [이번 장](#테스트가-가능하도록-코딩하라)

자동으로 테스트할 수 있는 방식으로 모든 코드를 작성하십시오.

#### 다른 사람이 자신의 코드를 테스트 가능하도록 코딩하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 원칙](#테스트-원칙) > [이번 장](#다른-사람이-자신의-코드를-테스트-가능하도록-코딩하라)

예를 들어 타시스템향 인터페이스를 추가하는 방식으로 다른 개발자들이 사용할 코드를 작성하는 경우, 다른 사람이 자신의 코드에 대한 단위 테스트를 작성할 수 있도록 하십시오. 

또한, 통합 테스트를 용이하게 하는 유용한 테스트 더블 제공하거나 종속성 반전을 활용하여 생산적인 설정을 테스트 설정으로 대체할 수 있습니다.

#### 가독성 확보를 위한 규칙들을 정하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 원칙](#테스트-원칙) > [이번 장](#가독성-확보를-위한-규칙들을-정하라)

테스트 코드를 생산적인 코드보다 훨씬 더 읽기 쉽게 만드십시오.
좋은 테스트로 나쁜 생산적인 코드를 다룰 수 있지만 테스트조차 받지 못하면 더 이상 할 수 있게 없습니다.

테스트 코드를 매우 간단하고 가볍게 작성하여 지금부터 1년 후에도 여전히 이해할 수 있도록 하십시오.

표준과 패턴을 고수하여 동료가 빠르게 코드를 이해할 수 있도록 하십시오.

#### 테스트 보고서를 복사하거나 작성하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 원칙](#테스트-원칙) > [이번 장](#테스트-보고서를-복사하거나-작성하지-마라)

개발 개체의 '$TMP' 복사본을 만들고 가지고 놀면서 백로그 항목에 대한 작업을 시작하지 마십시오.
다른 사람들은 이러한 개체를 알아차리지 못하므로 작업 상태를 알 수 없습니다.
처음에 작업 복사본을 만들면 아마도 많은 시간을 낭비하게 될 것입니다.
또한 나중에 복사본을 삭제하는 것을 잊어 시스템과 종속성을 망각할 수도 있습니다.
(믿기지 않으세요? 지금 바로 개발 시스템으로 가서 `$TMP`를 확인하세요.)

또한 특정 방식으로 무언가를 호출하는 테스트 보고서를 작성하여 시작하지 마십시오.
그리고, 작업할 때 작업이 계속 작동하는지 확인하지 마십시오.
테스트 보고서를 손으로 반복하고 모든 것이 여전히 괜찮은지 눈으로 확인하는 것은 가난한 사람(Poor Man)의 테스트입니다. 
자동 테스트 가정을 동반한 테스트들을 수행하고 보고서를 자동화하십시오. 

이는 두 가지 장점이 있는데, 
첫째, 나중에 단위 테스트를 작성해야 하는 수고를 덜어줄 것입니다.
둘째, 수동 반복에 많은 시간을 절약할 수 있을 뿐만 아니라 지루하고 지치는 일도 피할 수 있습니다.

#### Private가 아닌 Public으로 테스트하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 원칙](#테스트-원칙) > [이번 장](#Private가-아닌-Public으로-테스트하라)

클래스의 공개 부분, 특히 구현하는 인터페이스는 다소 안정적이며 변경될 가능성이 거의 없습니다.
단위 테스트를 public으로 작성하여 검증하고, 클래스를 리팩토링할 때 소비해야 하는 노력을 최소화하십시오.
이와 대조적으로 Protected 및 Private 는 리팩토링을 통해 매우 빠르게 변경될 수 있는데 이러한 각각의 리팩토링으로 인해 테스트가 불필요하게 중단될 수 있습니다.

Private 또는 Protected 메서드를 테스트해야 하는 긴급한 경우는 여러 종류의 설계 결함에 대한 조기 경고 신호일 수 있습니다. 

자신에게 질문해보십시오:

- 자체 테스트 모음과 함께 자체 클래스로 나오려는 개념들을 실수로 기존 클래스에 유지해버렸습니까?
  
- Glue Code에서 도메인 로직을 분리하는 것을 잊으셨나요? (역자 주 : Glue Code - 프로그램의 요구사항 구현에는 기여하지 않지만, 본래 호환성이 없는 부분끼리 결합하기 위해 작동하는 코드)
  예를 들어 작업, 결정 또는 유효성 검사로 BOPF에 연결되거나 `*_DPC_EXT` 데이터 공급자로 SAP Gateway에서 생성된 클래스에서 도메인 논리를 직접 구현하는 것은 최선의 생각이 아닐 수 있습니다.
  
- 인터페이스가 너무 복잡하고 관련이 없거나 쉽게 mocking 할 수 없는 너무 많은 데이터를 요청합니까?

#### 코드 커버리지에 집착하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 원칙](#테스트-원칙) > [이번 장](#코드-커버리지에-집착하지-마라)

코드 커버리지는 임의의 KPI를 충족하지 않고 테스트하는 것을 잊은 코드를 찾는 데 도움이 됩니다:

단지 커버리지를 높이기 위해 쓰레기 구문을 작성하지 마십시오.
안전하게 리팩토링할 수 없는 것을 투명하게 만들기 위해 테스트하지 않은 상태로 두는 것이 좋습니다.
100% 미만의 커버리지를 유지하면서도 완벽한 테스트를 수행할 수 있습니다.
테스트 더블을 삽입하기 위한 생성자의 IF와 같이 100%에 도달하는 것이 비실용적일 수 있는 경우가 있습니다.
좋은 테스트는 다른 분기와 조건에 대해 동일한 명령문을 여러 번 다루는 경향이 있습니다.
그들은 실제로 100% 이상의 적용 범위를 가질 것입니다.

### 테스트 클래스

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#테스트 클래스)

#### 목적에 따라 로컬 테스트 클래스 호출하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 클래스](#테스트-클래스) > [이번 장](#목적에-따라-로컬-테스트-클래스-호출하라)

테스트 시나리오의 "언제" 부분으로 로컬 테스트 클래스의 이름을 지정하십시오.

```ABAP
CLASS ltc_<public method name> DEFINITION FOR TESTING ... ."
```

또는 "주어진 상황" 으로 이름을 지정해도 됩니다.

```ABAP
CLASS ltc_<common setup semantics> DEFINITION FOR TESTING ... .
```

```ABAP
" 잘못된 패턴s
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " We know that's the class under test - why repeat it?
CLASS ltc_test DEFINITION FOR TESTING ....                      " Of course it's a test, what else should it be?
```

#### 로컬 클래스에 단위 테스트를 포함시켜라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 클래스](#테스트-클래스) > [이번 장](#로컬-클래스에-단위-테스트를-포함시켜라)

로컬 테스트에(테스트 중인 클래스를 포함) 단위 테스트를 넣으십시오.

이렇게 하면 [테스트 클래스를 실행하는 방법](#테스트-클래스를-실행하는-방법)에 설명된 대로 사람들이 클래스를 리팩토링할 때 이러한 테스트를 찾을 수 있고 한 번의 키 누름으로 모든 관련 테스트를 실행할 수 있습니다.

구성 요소, 통합 및 시스템 테스트를 별도의 전역 클래스에 포함된 로컬 테스트에 넣으십시오.
이들은 테스트 중인 단일 클래스와 직접 관련이 없으므로 임의로 지정해서는 안 됩니다. 관련된 클래스 중 하나에 배치되지만 별도의 클래스에 배치됩니다. 
프로덕션 코드에서 실수로 참조되는 것을 방지하려면 이 글로벌 테스트 클래스를 `FOR TESTING` 및 `ABSTRACT`로 표시하세요.
테스트를 다른 클래스에 넣는 것은 사람들이 관련 클래스를 리팩토링할 때 테스트를 간과하고 실행하는 것을 잊어버릴 위험이 있습니다.

따라서 *테스트 관계*를 사용하여 테스트에 의해 테스트되는 개체를 문서화하는 것이 좋습니다.
아래 예시에서 테스트 클래스 `hiring_test`는 단축키 `Shift-Crtl-F12`(Windows) 또는 `Cmd-Shift-F12`(macOS)를 통해 `recruting` 또는 `candidate` 클래스를 테스트할 수 있습니다.

```abap
"! @testing recruting
"! @testing candidate
class hiring_test definition
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```

#### 도움말 클래스에 도움말 메서드를 포함시켜라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 클래스](#테스트-클래스) > [이번 장](#도움말-클래스에-도움말-메서드를-포함시켜라)

여러 테스트 클래스에서 사용하는 도움말 메서드를 도움말 클래스에 넣습니다. 상속(is-relation) 또는 위임(has-relation)을 통해 도움말 메서드를 사용할 수 있도록 합니다.

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

#### 테스트 클래스를 실행하는 방법 

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 클래스](#테스트-클래스) > [이번 장](#테스트-클래스를-실행하는-방법)

ABAP 개발 도구에서 Ctrl+Shift+F10을 눌러 클래스의 모든 테스트를 실행하십시오
적용 범위 측정을 포함하려면 Ctrl+Shift+F11을 누르십시오.
테스트 관계로 유지되는 다른 클래스에서도 테스트를 실행하려면 Ctrl+Shift+F12를 누르십시오.

> mac에서는 `Ctrl` 대신에 `Cmd` 버튼을 사용하십시오.

### 테스트 중인 코드

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#테스트-중인-코드)

#### 테스트 중인 코드를 의미 있게 네이밍하거나 CUT 코드를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 중인 코드](#테스트-중인-코드) > [이번 장](#테스트-중인-코드를-의미-있게-네이밍하거나-CUT-코드를-사용하라)

테스트 중인 코드를 나타내는 변수에 의미 있는 이름을 지정합니다:

```ABAP
DATA blog_post TYPE REF TO ...
```

가치가 없는 모든 네임스페이스와 접두사를 사용하여 클래스 이름을 반복하지 마십시오.

```ABAP
" 잘못된 패턴
DATA clean_fra_blog_post TYPE REF TO ...
```

다른 테스트 설정이 있는 경우 개체의 다양한 상태를 설명하는 것이 도움이 될 수 있습니다:

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

의미 있는 이름을 찾는 데 문제가 있으면 `cut`를 기본값으로 사용하세요.
이는 "테스트 중인 코드"를 나타냅니다.

```ABAP
DATA cut TYPE REF TO ...
```

특히 깔끔하지 못하고 혼란스러운 테스트에서 변수 `cut`을 호출하면 일시적으로 독자가 실제로 테스트한 내용을 확인하는 데 도움이 될 수 있습니다.
그러나 테스트를 정리하는 것은 장기적으로 실제로 가야 할 길입니다.

#### 클래스 구현이 아닌 인터페이스를 테스트하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 중인 코드](#테스트-중인-코드) > [이번 장](#클래스-구현이-아닌-인터페이스를-테스트하라)

 [Private가 아닌 Public으로 테스트하라](#Private가-아닌-Public으로-테스트하라)의 실용적인 예로, _interface_를 사용하여 테스트 중인 코드를 입력하세요.

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

_class_를 사용하는 것을 지양하십시오.

```ABAP
" 잘못된 패턴
DATA code_under_test TYPE REF TO some_class.
```

#### 테스트 중인 코드에 대한 호출을 자체 메서드로 추출하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 중인 코드](#테스트-중인-코드) > [이번 장](#테스트-중인-코드에-대한-호출을-자체-메서드로-추출하라)

테스트할 방법에 많은 파라미터 또는 준비된 데이터가 필요한 경우, 이는 사용하지 않는 파라미터를 기본값으로 사용하는 자체 메서드에 대한 호출을 추출하는 데 도움이 될 수 있습니다:

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

원래 메서드를 직접 호출하면 테스트가 무의미한 세부 정보로 가득 차게 될 수 있습니다:

```ABAP
" 잘못된 패턴
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### 인젝션

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#인젝션)

#### 테스트 더블을 인젝션할 때 종속성 반전을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#테스트-더블을-인젝션할-때-종속성-반전을-사용하라)

종속성 반전은 모든 종속성을 생성자에게 넘겨주는 것을 의미합니다:

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

Setter 인젝션을 사용하지 마십시오.
이는 개발자가 의도하지 않은 방식으로 생산적인 코드를 사용하게 만들어버립니다:

```ABAP
" 잘못된 패턴
METHODS set_customizing_reader
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD do_something.
  object->set_customizing_reader( a ).
  object->set_customizing_reader( b ). " would you expect that somebody does this?
ENDMETHOD.
```

FRIENDS 인젝션을 사용하지 마십시오.
이는 예상하지 못한 결과로 대체되기 전에 생산적 종속성을 초기화합니다.
이것들은 내부 이름을 바꾸면 바로 중단될 것입니다.
또한 생성자의 초기화를 우회합니다.

```ABAP
" 잘못된 패턴
METHOD setup.
  cut = NEW fra_my_class( ). " <- builds a productive customizing_reader first - what will it break with that?
  cut->customizing_reader ?= cl_abap_testdouble=>create( 'if_fra_cust_obj_model_reader' ).
ENDMETHOD.

METHOD constructor.
  customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
  customizing_reader->fill_buffer( ). " <- won't be called on your test double, so no chance to test this
ENDMETHOD.
```

#### ABAP 테스트 더블 툴 사용을 고려하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#ABAP-테스트-더블-툴-사용을-고려하라)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

사용자 정의 테스트 두 배보다 짧고 이해하기 쉽습니다:

```ABAP
" 잘못된 패턴
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

#### 테스트 도구를 활용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#테스트-도구를-활용하라)

일반적으로 깔끔한 프로그래밍 스타일을 사용하면 표준 ABAP 단위 테스트 및 테스트 더블로 많은 작업을 수행할 수 있습니다.
그러나 더 까다로운 경우를 우아한 방식으로 처리할 수 있는 도구가 있습니다.

-  시스템의 나머지 부분을 방해하지 않고 테스트 데이터로 채울 수 있는 테스트 데이터 저장소로 리디렉션하여 복잡한 OpenSQL 문을 테스트하려면 `CL_OSQL_REPLACE` 서비스를 사용하세요.
  
- CDS 테스트 프레임워크를 사용하여 CDS 보기를 테스트하십시오.

#### 테스트 이음새는 임시 해결책으로만 활용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#테스트-이음새는-임시-해결책으로만-활용하라)

다른 모든 기술이 실패하거나 위험하고 얕은 레거시 코드에 있는 경우, 테스트를 억지로 가능하게 하는 [테스트 이음새](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abaptest-seam.htm)를 삼가세요.

언뜻 보기에는 편안해 보이지만 테스트 이음새는 종속성에 얽히는 경향이 있어 장기적으로 안정적이고 안정적으로 유지하기 어렵습니다.

따라서 코드를 더 테스트 가능한 형식으로 리팩토링할 수 있도록 임시 해결 방법으로만 이음새를 테스트하지 않는 것이 좋습니다.

#### 종속성 반전 생성자에 접근할 때는 LOCAL FRIENDS를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#종속성-반전-생성자에-접근할-때는-LOCAL-FRIENDS를-사용하라)

```ABAP
저도 CLASS /clean/unit_tests DEFINITION.
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

#### 테스트된 코드에 영향을 주면서까지 LOCAL FRIENDS를 오용하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#테스트된-코드에-영향을-주면서까지-LOCAL-FRIENDS를-오용하지-마라)

mocking된 데이터를 삽입하기 위해 private 및 protected 멤버에 액세스하는 단위 테스트는 취약합니다.
그들은 테스트된 코드의 내부 구조가 변경되면 중단됩니다.

```ABAP
" 잘못된 패턴
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### 코드를 테스트 가능하도록 만들기 위해 기존의 코드를 변경하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#코드를-테스트-가능하도록-만들기-위해-기존의-코드를-변경하지-마라)

```ABAP
" 잘못된 패턴
IF me->in_test_mode = abap_true.
```

#### 메서드를 mocking하기 위해 sub-class를 만들지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#메서드를-mocking하기-위해-sub-class를-만들지-마라)

단위 테스트에서 mocking하기 위해 메서드를 하위 클래스로 만들고 덮어쓰지 마십시오.
이것은 작동하지만 코드를 리팩토링할 때 테스트가 쉽게 중단되기 때문에 취약합니다.
또한 실제 개발자가 클래스를 상속할 수 있도록 해주는데 이때의 클래스는 [상속을-고려하지-않았다면-FINAL을-사용하라](#상속을-고려하지-않았다면-FINAL을-사용하라) 에서 언급했던 특징을 가지고 있습니다.

```ABAP
" 잘못된 패턴
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].
  PROTECTED SECTION.
    METHODS needs_to_be_mocked REDEFINITION.
```

테스트 중인 레거시 코드를 가져오려면 [테스트 이음새는 임시 해결책으로만 활용하라](#테스트-이음새는-임시-해결책으로만-활용하라)를 참고하십시오.
이러한 방법은 취약하지만 여전히 깔끔한 방법인데, 이전 `FINAL` 플래그를 제거하거나 메서드 범위를 `PRIVATE`에서 `PROTECTED`로 변경하여 상속을 활성화할 때 발생하는 것처럼 최소한 클래스의 생산적인 동작을 변경하지 않기 때문입니다.

새 코드를 작성할 때 클래스를 설계할 때 이 테스트 가능성 문제를 직접 고려하십시오. 다른 더 나은 방법을 찾으십시오.
일반적인 모범 사례에는 [테스트 도구를 활용하라](#테스트-도구를-활용하라)의 케이스 및 자체 인터페이스가 있는 별도의 클래스로 문제 메서드를 추출하는 것이 포함됩니다.

> [코드를 테스트 가능하도록 만들기 위해 기존의 코드를 변경하지 마라](#코드를-테스트-가능하도록-만들기-위해-기존의-코드를-변경하지-마라)의 보다 구체적인 변형입니다.

#### 필요하지 않은 것까지 mocking하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#필요하지-않은-것까지-mocking하지-마라)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

주어진 것을 가능한 한 정확하게 정의하십시오: 테스트에 필요하지 않은 데이터를 설정하지 말고 호출되지 않은 객체를 mocking하지 마십시오.
이러한 것들은 실제로 일어나고 있는 일로부터 개발자의 주의를 산만하게 합니다.

```ABAP
" 잘못된 패턴
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

어떤 것을 mocking할 필요가 전혀 없는 경우도 있습니다. 이것은 일반적으로 데이터 구조와 데이터 컨테이너의 경우입니다.
예를 들어 단위 테스트는 부작용 없이 데이터만 저장하기 때문에 `transient_log`의 생산적인 버전에서 잘 작동할 수 있습니다.

#### 테스트 프레임워크를 구축하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [인젝션](#인젝션) > [이번 장](#테스트-프레임워크를-구축하지-마라)

통합 테스트와 달리 단위 테스트는 데이터 입력 데이터 출력이어야 하며 모든 테스트 데이터는 필요에 따라 즉석에서 정의해야 합니다.

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

제공할 데이터를 결정하기 위해 "*테스트 케이스 ID*"를 구분하는 프레임워크 구축을 시작하지 마십시오.
결과 코드는 너무 길고 엉켜서 이러한 테스트를 장기적으로 유지할 수 없습니다.

```ABAP
" 잘못된 패턴

test_double->set_test_case( 1 ).

CASE me->test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### 테스트 메서드

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#테스트-메서드)

#### 상황이 잘 묘사되도록 테스트 메서드를 네이밍하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 메서드](#테스트-메서드) > [이번 장](#상황이-잘-묘사되도록-테스트-메서드를-네이밍하라)

좋은 메스드명은 given, then이 반영되어 있습니다:

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

나쁜 이름은 시기를 반영하거나 의미 없는 사실을 반복하거나 암호처럼 되어 있습니다:

```ABAP
" 잘못된 패턴s

" What's expected, success or failure?
METHOD get_conversion_exits.

" It's a test method, what else should it do but "test"?
METHOD test_loop.

" So it's parameterized, but what is its aim?
METHOD parameterized_test.

" What's "_wo_w" supposed to mean and will you still remember that in a year from now?
METHOD get_attributes_wo_w.
```

ABAP는 메서드 이름에 30자만 허용하므로 이름이 너무 짧아서 충분한 의미를 전달할 수 없는 경우 설명을 추가하는 것이 좋습니다.
 ABAP Doc 또는 테스트 방법의 첫 번째 줄이 주석에 대한 적절한 선택일 수 있습니다.

이름이 너무 긴 테스트 메서드가 많다는 것은 단일 테스트 클래스를 여러 클래스로 분할하고 클래스 이름에서 주어진 차이점을 표현해야 한다는 표시일 수 있습니다.

#### given-when-then을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 메서드](#테스트-메서드) > [이번 장](#given--when--then을-사용하라)

Given-When-Then 패러다임에 따라 테스트 코드를 구성하십시오:
먼저 주어진 섹션("Given")에서 항목을 초기화하고,
두 번째로 실제 테스트된 것을 호출합니다("When").
세 번째로 결과를 확인합니다("Then").

Given 또는 Then 섹션이 너무 길어서 더 이상 세 섹션을 시각적으로 구분할 수 없으면 하위 메서드를 추출하십시오.
구분 기호로 사용되는 빈 줄이나 주석은 언뜻 보기에는 좋아 보일 수 있지만 시각적 혼란을 줄이는 것은 아닙니다.
여전히 그들은 개발자와 초보 테스트 작성자가 섹션을 분리하는 데 도움이 됩니다.

#### when 구문을 사용할 때는 정확히 하나만을 호출하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 메서드](#테스트-메서드) > [이번 장](#when-구문을-사용할-때는-정확히-하나만을-호출하라)

테스트 메서드의 "When" 섹션에 테스트 중인 클래스에 대한 호출이 정확히 한 번 포함되어 있는지 확인하십시오.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

여러 항목을 호출하는 것은 메서드에 명확한 초점이 없고 테스트가 너무 많다는 것을 나타냅니다.
이렇게 하면 테스트가 실패할 때 원인을 찾기가 더 어려워집니다:
실패의 원인이 첫 번째, 두 번째 또는 세 번째 호출이었습니까?
또한 테스트 중인 정확한 기능이 무엇인지 확신하지 못하기 때문에 개발자를 혼란스럽게 합니다.

#### 정말 필요한 경우가 아니라면 TEARDOWN을 추가하지 마라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 메서드](#테스트-메서드) > [이번 장](#정말-필요한-경우가-아니라면-TEARDOWN을-추가하지-마라)

`teardown` 방법은 일반적으로 통합 테스트에서 데이터베이스 항목 또는 기타 외부 리소스를 정리하는 데만 필요합니다.

테스트 클래스의 멤버 재설정, 특히 `cut` 과 테스트 더블은 불필요합니다;
그들은 다음 테스트 방법이 시작되기 전에 `setup` 방법으로 덮어씁니다.

### 테스트 데이터

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#테스트-데이터)

#### 의미를 쉽게 이해할 수 있도록 테스트 데이터를 만들어라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 데이터](#테스트-데이터) > [이번 장](#의미를-쉽게-이해할-수-있도록-테스트-데이터를-만들어라)

단위 테스트에서 당신은 어떤 데이터와 테스트더블이 중요한지, 그리고 어떤 것들이 코드 충돌을 방지할 수 있는지 빠르게 말할 수 있기를 원할 것입니다.
의미가 없는 것에 명확한 이름과 값을 지정하여 이를 지원하십시오. 예를 들면 다음과 같습니다:

```ABAP
DATA(alert_id) = '42'.                             " well-known meaningless numbers
DATA(detection_object_type) = '?=/"&'.             " 'keyboard accidents'
CONSTANTS some_random_number TYPE i VALUE 782346.  " revealing variable names
```

사람들이 무언가가 실제 개체에 연결되어 있다고 믿도록 속이거나 그렇지 않은 경우 실제 커스터마이징을 하지 마십시오:

```ABAP
" 잘못된 패턴
DATA(alert_id) = '00000001223678871'.        " this alert really exists
DATA(detection_object_type) = 'FRA_SCLAIM'.  " this detection object type, too
CONSTANTS memory_limit TYPE i VALUE 4096.    " this number looks carefully chosen
```

#### 차이점을 쉽게 이해할 수 있도록 테스트 데이터를 만들어라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 데이터](#테스트-데이터) > [이번 장](#차이점을-쉽게-이해할-수-있도록-테스트-데이터를-만들어라)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

개발자에게 작은 차이를 찾기 위해 의미 없는 긴 문자열을 비교하도록 강요하지 마십시오.

#### 테스트 데이터의 목적과 중요성을 설명할 때는 상수를 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [테스트 데이터](#테스트-데이터) > [이번 장](#테스트-데이터의-목적과-중요성을-설명할-때는-상수를-사용하라)

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

### 어써션

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [이번 장](#어써션)

#### 어써션을 최소화하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#어써션을-최소화하라)

Assert only exactly what the test method is about, and this with a small number of assertions. 테스트 메서드가 어떤 것에 대한 것인지 정확히 Assert 하고 이때 Assert 수의 최소화 하십시오.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

너무 많이 Assert하는 것은 메서드에 명확한 초점이 없다는 표시입니다.
이것은 너무 많은 곳에서 생산적인 코드와 테스트 코드를 결합합니다. 기능을 변경하려면 변경된 기능과 실제로 관련되지는 않지만 많은 수의 테스트를 다시 작성해야 합니다.
그것은 또한 개발자들을 매우 다양한 주장으로 혼란스럽게 하고, 그들 사이에서 중요하고 구별되는 주장을 흐리게 합니다.

```ABAP
" 잘못된 패턴
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

#### 올바른 어써션 유형을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#올바른-어써션-유형을-사용하라)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

Assert는 종종 눈에 보이는 것 이상을 수행합니다. 예를 들어 `assert_equals`는 Type 일치여부를 확인하고 값이 다를 경우 정확한 설명을 제공합니다.
잘못되거나 너무 흔한 Assert를 사용하면 오류 메시지에서 무엇이 잘못되었는지 바로 알 수 있게 하는 대신 즉시 디버거로 들어가게 됩니다.

```ABAP
" 잘못된 패턴
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### 수량이 아닌 내용을 어쎠선하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#수량이-아닌-내용을-어쎠선하라)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

당신이 기대하는 실제 내용을 표현할 수 있다면 magic-number-quantity Assert를 작성하지 마십시오.
기대치는 충족되지만 수치는 다를 수 있습니다.
반대로 내용이 완전히 예상치 못한 것이지만 숫자는 일치할 수 있습니다.

```ABAP
" 잘못된 패턴
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### 내용이 아닌 품질을 어써션하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#내용이-아닌-품질을-어써션하라)

결과의 메타 품질에 관심이 있지만 실제 콘텐츠 자체에는 관심이 없다면 적절한 Assert로 이를 표현하십시오:

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

정확한 내용을 Assert하면, 실제로 테스트하려는 내용을 애매하게 합니다.
또한 리팩토링은 서로 다르지만 완벽하게 수용 가능한 결과를 생성할 수도 있습니다.

```ABAP
" 잘못된 패턴
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### 예외를 처리할 때는 FAIL 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#예외를-처리할-때는-FAIL-구문을-사용하라)

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

#### Try-Catch 구문을 사용하는 대신에 when-then 구문을 사용하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#Try-Catch-구문을-사용하는-대신에-when-then-구문을-사용하라)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

위 테스트 코드는 Happy-Path에 초점을 맞추고 있으므로 다음과 비교하여 훨씬 더 읽기 쉽고 이해하기 쉽습니다:

```ABAP
" 잘못된 패턴
METHOD reads_entry.
  TRY.
      DATA(entry) = cut->read_something( ).
    CATCH /clean/some_exception INTO DATA(unexpected_exception).
      cl_abap_unit_assert=>fail( unexpected_exception->get_text( ) ).
  ENDTRY.
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

#### 코드를 줄이고 중복을 피하기 위해 사용자 지정 어써션을 작성하라

> [Clean ABAP](#clean-abap) > [내용](#내용) > [테스트](#테스트) > [어써션](#어써션) > [이번 장](#코드를-줄이고-중복을-피하기-위해-사용자-지정-어써션을-작성하라)

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

이것을 계속해서 복사하여 붙여넣는 것을 지양하십시오.
