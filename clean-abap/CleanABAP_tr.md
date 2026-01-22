# Clean ABAP

> [**Türkçe**](CleanABAP_tr.md)
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
> &nbsp;·&nbsp;
> [Türkçe](CleanABAP_tr.md)

Bu rehber, 
[Robert C. Martin's _Clean Code_]
prensiplerinin [ABAP](https://en.wikipedia.org/wiki/ABAP) dili için uyarlanmış halidir.

Bu [Cheat Sheet](cheat-sheet/CheatSheet.md) baskı almak için optimize edilmiştir.

[Robert C. Martin's _Clean Code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## İçerik

- [Nasıl Yapılır](#nasıl-yapılır)
  - [Clean Code yazmaya nasıl başlanır](#clean-code-yazmaya-nasıl-başlanır)
  - [Eski (Legacy) kodu nasıl refaktör edersiniz](#eski-legacy-kodu-nasıl-refaktör-edersiniz)
  - [Otomatik nasıl kontrol edersiniz](#otomatik-nasıl-kontrol-edersiniz)
  - [Diğer rehberlerle nasıl ilişkilendirilir](#diğer-rehberlerle-nasıl-ilişkilendirilir)
  - [Nasıl karşı çıkmalısınız](#nasıl-karşı-çıkmalısınız)
- [İsimlendirme](#isimlendirme)
  - [Açıklayıcı isimler kullanın](#açıklayıcı-isimler-kullanın)
  - [Çözüm alanı ve problem alanı terimlerini tercih edin](#çözüm-alanı-ve-problem-alanı-terimlerini-tercih-edin)
  - [Çoğul eklerini kullanın](#çoğul-eklerini-kullanın)
  - [Okunabilir isimler kullanın](#okunabilir-isimler-kullanın)
  - [snake_case kullanın](#snake_case-kullanın)
  - [Kısaltmalardan kaçının](#kısaltmalardan-kaçının)
  - [Her yerde aynı kısaltmaları kullanın](#her-yerde-aynı-kısaltmaları-kullanın)
  - [Sınıflar için isim, Metodlar için fiil kullanın](#sınıflar-için-isim-metodlar-için-fiil-kullanın)
  - ["data", "info", "object" gibi belirsiz kelimelerden kaçının](#data-info-object-gibi-belirsiz-kelimelerden-kaçının)
  - [Her kavram için bir kelime kullanın](#her-konsept-için-bir-kelime-kullanın)
  - [Desen (Pattern) adlarını yalnızca onları kastediyorsanız kullanın](#desen-pattern-adlarını-yalnızca-onları-kastediyorsanız-kullanın)
  - [Öneklerden kaçının, özellikle Macar notasyonu kullanmaktan](#öneklerden-kaçının-özellikle-macar-notasyonu-kullanmaktan)
  - [Yerleşik Fonksiyonları Gizlemekten Kaçının](#yerleşik-fonksiyonları-gizlemekten-kaçının)
- [Dil](#dil)
  - [Eski (Legacy) koda dikkat edin](#eski-legacy-koda-dikkat-edin)
  - [Performansa dikkat edin](#performansa-dikkat-edin)
  - [Nesne yönelimli programlamayı prosedürel programlamaya tercih edin](#nesne-yönelimli-programlamayı-prosedürel-programlamaya-tercih-edin)
  - [Fonksiyonel yapıları, prosedürel dil yapılarına tercih edin](#fonksiyonel-yapıları-prosedürel-dil-yapılarına-tercih-edin)
  - [Eski dönem dil elemanlarından kaçının](#eski-dönem-dil-elemanlarından-kaçının)
  - [Tasarım desenlerini akıllıca kullanın](#tasarım-desenlerini-akıllıca-kullanın)
- [Sabitler (Constants)](#sabitler-constants)
  - [Sihirli sayılar yerine sabit değerler kullanın](#sihirli-sayılar-yerine-sabit-değerler-kullanın)
  - [Sabit değerlerin de açıklayıcı isimlere ihtiyacı vardır](#sabit-değerlerin-de-açıklayıcı-isimlere-ihtiyacı-vardır)  
  - [Sabit değer arayüzlerine karşı ENUM tercih edin](#sabit-değer-arayüzlerine-karşı-enum-tercih-edin)
  - [ENUM veya Enumeration deseni kullanmazsanız, sabit değerlerinizi gruplandırın](#enum-veya-enumeration-deseni-kullanmazsanız-sabit-değerlerinizi-gruplandırın)
- [Değişkenler](#değişkenler)
  - [Önceden tanımlanmış değişkenler yerine satır içi değişken kullanımını tercih edin](#önceden-tanımlanmış-değişkenler-yerine-satır-içi-değişken-kullanımını-tercih-edin)
  - [Değişkenleri tanımlandıkları ifade (statement) bloğu dışında kullanmayın](#değişkenleri-tanımlandıkları-ifade-statement-bloğu-dışında-kullanmayın)
  - [Değişken tanımlarını zincirlemeyin](#değişken-tanımlarını-zincirlemeyin)
  - [Dinamik veri erişimi için field symbol kullanmayın](#dinamik-veri-erişimi-için-field-symbol-kullanmayın)
  - [Döngüleriniz için doğru hedefleri seçin](#döngüleriniz-için-doğru-hedefleri-seçin)
- [Tablolar](#tablolar)
  - [Uygun tablo türünü kullanmayı tercih edin](#uygun-tablo-türünü-kullanmayı-tercih-edin)
  - [DEFAULT KEY kullanımından kaçının](#default-key-kullanımından-kaçının)
  - [APPEND TO yerine INSERT INTO TABLE kullanmayı tercih edin](#append-to-yerine-insert-into-table-kullanmayı-tercih-edin)
  - [READ TABLE veya LOOP AT yerine LINE_EXISTS kullanmayı tercih edin](#read-table-veya-loop-at-yerine-line_exists-kullanmayı-tercih-edin)
  - [LOOP AT yerine READ TABLE kullanmayı tercih edin](#loop-at-yerine-read-table-kullanmayı-tercih-edin)
  - [İç içe IF yapıları yerine LOOP AT WHERE kullanmayı tercih edin](#iç-içe-if-yapıları-yerine-loop-at-where-kullanmayı-tercih-edin)
  - [Gereksiz tablo okumalarından kaçının](#gereksiz-tablo-okumalarından-kaçının)
- [String Değerler](#string-değerler)
  - [Sabit değerleri tanımlarken ` kullanın](#sabit-değerleri-tanımlarken-kullanın)
  - [Metinleri birleştirmek için | kullanın](#metinleri-birleştirmek-için-kullanın)
- [Boolean Değerler](#boolean-değerler)
  - [Boolean'ları bilinçli kullanın](#booleanları-bilinçli-kullanın)
  - [Boolean'lar için ABAP_BOOL kullanın](#booleanlar-için-abap_bool-kullanın)
  - [Karşılaştırmalar için ABAP_TRUE ve ABAP_FALSE kullanın](#karşılaştırmalar-için-abap_true-ve-abap_false-kullanın)
  - [Boolean değişken atamalarında XSDBOOL kullanın](#boolean-değişken-atamalarında-xsdbool-kullanın)
- [Koşullar (Conditions)](#koşullar-conditions)
  - [Koşulları mümkün olduğunca olumlu yazmaya çalışın](#koşulları-mümkün-olduğunca-olumlu-yazmaya-çalışın)
  - [NOT IS yerine IS NOT tercih edin](#not-is-yerine-is-not-tercih-edin)
  - [Boolean metodlar için kestirimsel (predicative) çağrılar kullanmayı düşünün](#boolean-metodlar-için-kestirimsel-predicative-çağrılar-kullanmayı-düşünün)
  - [Karmaşık koşulları ayrıştırın](#karmaşık-koşulları-ayrıştırın)
  - [Karmaşık koşulları ayıklayın](#karmaşık-koşulları-ayıklayın)
- [If Yapıları](#if-yapıları)
  - [Boş IF blokları kullanmayın](#boş-if-blokları-kullanmayın)
  - [Çoklu alternatif durumlar için ELSE IF yerine CASE tercih edin](#çoklu-alternatif-durumlar-için-else-if-yerine-case-tercih-edin)
  - [İç içe IF yapılarının derinliğini azaltın](#iç-içe-if-yapılarının-derinliğini-azaltın)
- [Düzenli İfadeler (Regex)](#düzenli-ifadeler-regex)
  - [Düzenli ifadeler yerine daha basit yöntemleri tercih edin](#düzenli-ifadeler-yerine-daha-basit-yöntemleri-tercih-edin)
  - [Regex yerine temel kontrolleri (basis checks) tercih edin](#regex-yerine-temel-kontrolleri-basis-checks-tercih-edin)
  - [Karmaşık düzenli ifadeleri oluştururken parça parça birleştirmeyi düşünün](#karmaşık-düzenli-ifadeleri-oluştururken-parça-parça-birleştirmeyi-düşünün)
- [Sınıflar](#sınıflar)
  - [Sınıflar: Nesne Yönelimli yapı](#sınıflar-nesne-yönelimli-yapı)
    - [Statik sınıflar yerine nesneleri tercih edin](#statik-sınıflar-yerine-nesneleri-tercih-edin)
    - [Kalıtım (inheritance) yerine bileşimi (composition) tercih edin](#kalıtım-inheritance-yerine-bileşimi-tercih-edin)
    - [Aynı sınıf içerisinde durum tutan (stateful) ve durumsuz (stateless) yapıları karıştırmayın](#aynı-sınıf-içerisinde-durum-tutan-stateful-ve-durumsuz-stateless-yapıları-karıştırmayın)
  - [Kapsam (Scope)](#kapsam-scope)
    - [Varsayılan olarak global tanımlayın, sadece gerektiğinde lokal kullanın](#varsaylın-olarak-global-tanımlayın-sadece-gerektiğinde-lokal-kullanın)
    - [Eğer sınıf kalıtım (inheritance) için tasarlanmadıysa FINAL olarak işaretleyin](#eğer-sınıf-kalıtım-inheritance-için-tasarlanmadıysa-final-olarak-işaretleyin)
    - [Sınıf üyelerini varsayılan olarak PRIVATE yapın, sadece gerektiğinde PROTECTED kullanın](#sınıf-üyelerini-varsayılan-olarak-private-yapın-sadece-gerektiğinde-protected-kullanın)
    - [Getter yerine değiştirilemez (immutable) veri kullanmayı düşünün](#getter-yerine-değiştirilemez-immutable-veri-kullanmayı-düşünün)
    - [READ-ONLY ifadesini dikkatli kullanın](#read-only-ifadesini-dikkatli-kullanın)
  - [Yapılandırıcı (Constructors)](#yapılandırıcı-constructors)
    - [CREATE OBJECT yerine NEW kullanmayı tercih edin](#create-object-yerine-new-kullanmayı-tercih-edin)
    - [Global sınıfınız CREATE PRIVATE ise, CONSTRUCTOR metodunu PUBLIC bırakın](#global-sınıfınız-create-private-ise-constructor-metodunu-public-bırakın)
    - [Opsiyonel parametreler yerine birden fazla statik oluşturucu (creation) metodu kullanmayı tercih edin](#opsiyonel-parametreler-yerine-birden-fazla-statik-oluşturucu-creation-metodu-kullanmayı-tercih-edin)
    - [Birden fazla oluşturucu metod varsa açıklayıcı isimler verin](#birden-fazla-oluşturucu-metod-varsa-açıklayıcı-isimler-verin)
    - [Sadece birden fazla örnek anlamsızsa singleton sınıf oluşturun](#sadece-birden-fazla-örnek-anlamsızsa-singleton-sınıf-oluşturun)
- [Metotlar](#metodlar)
  - [Çağrılar (Calls)](#çağrılar-calls)
    - [Statik metodları nesne değişkenleri (instance variables) üzerinden çağırmayın](#statik-metodları-nesne-değişkenleri-instance-variables-üzerinden-çağırmayın)
    - [Tip tanımlarına nesne değişkenleri (instance variables) üzerinden çağırmayın](#tip-tanımlarına-nesne-değişkenleri-instance-variables-üzerinden-çağırmayın)
    - [İşlevsel (functional) çağrıları prosedürel (procedural) çağrılara tercih edin](#işlevsel-functional-çağrıları-prosedürel-procedural-çağrılara-tercih-edin)
    - [RECEIVING anahtar kelimesini kullanmayın](#receiving-anahtar-kelimesini-kullanmayın)
    - [Opsiyonel olan EXPORTING anahtar kelimesini atlayın](#opsiyonel-olan-exporting-anahtar-kelimesini-atlayın)
    - [Tek parametreli çağrılarda parametre ismini yazmayın](#tek-parametreli-çağrılarda-parametre-ismini-yazmayın)
    - [Bir örnek niteliği (attribute) veya metodu çağırırken ME öz referansını kullanmayın](#bir-örnek-niteliği-attribute-veya-metodu-çağırırken-me-öz-referansını-kullanmayın)
  - [Metotlar: Nesne Yönelimli (Object Orientation)](#metodlar-nesne-yönelimli-object-orientation)
    - [Statik metodlar yerine örnek (instance) metodları tercih edin](#statik-metodlar-yerine-örnek-instance-metodları-tercih-edin)
    - [Herkese açık (public) örnek (instance) metodlar bir arayüzün (interface) parçası olmalıdır](#herkese-açık-public-örnek-instance-metodlar-bir-arayüzün-interface-parçası-olmalıdır)
  - [Parametre Sayısı](#parametre-sayısı)
    - [IMPORTING parametre sayısını az tutmaya çalışın, ideal olarak üçten az olsun](#importing-parametre-sayısını-az-tutmaya-çalışın-ideal-olarak-üçten-az-olsun)
    - [Opsiyonel parametreler eklemek yerine metodları bölün](#opsiyonel-parametreler-eklemek-yerine-metodları-bölün)
    - [PREFERRED PARAMETER ifadesini çok nadiren kullanın](#preferred-parameter-ifadesini-çok-nadiren-kullanın)
    - [RETURN, EXPORT veya CHANGE işlemlerinden yalnızca bir parametre üzerinde işlem yapın](#return-export-veya-change-işlemlerinden-yalnızca-bir-parametre-üzerinde-işlem-yapın)
  - [Parametre Tipleri](#parametre-tipleri)
    - [EXPORTING yerine RETURNING kullanmayı tercih edin](#exporting-yerine-returning-kullanmayı-tercih-edin)
    - [RETURNING ile büyük tablolar döndürmek genelde sorun değildir](#retruning-ile-büyük-tablolar-döndürmek-genelde-sorun-değildir)
    - [RETURNING, EXPORTING veya CHANGING parametrelerinden yalnızca birini kullanın, kombinasyon yapmayın](#returning-exporting-veya-changing-parametrelerinden-yalnızca-birini-kullanın-kombinasyon-yapmayın)
    - [CHANGING parametrelerini sadece uygun olduğu yerlerde ve dikkatli kullanın](#changing-parametrelerini-sadece-uygun-olduğu-yerlerde-ve-dikkatli-kullanın)
    - [Boolean giriş parametresi kullanmak yerine metodu bölün](#boolean-giriş-parametresi-kullanmak-yerine-metodu-bölün)
  - [Parametre İsimleri](#parametre-isimleri)
    - [RETURNING parametresine RESULT adını vermeyi düşünün](#returning-parametresine-result-adını-vermeyi-düşünün)
  - [Parametre Başlatma (Initialization)](#parameter-başlatma-initialization)
    - [EXPORTING referans parametrelerini temizleyin veya üzerine yazın](#exporting-referans-parametrelerini-temizleyin-veya-üzerine-yazın)
      - [Girdi ve çıktının aynı olabileceği durumlarda dikkatli olun](#girdi-ve-çıktının-aynı-olabileceği-durumlarda-dikkatli-olun)
    - [VALUE parametrelerini temizlemeyin](#value-parametrelerini-temizlemeyin)
  - [Metot Gövdesi](#metod-gövdesi)
    - [Tek bir işi yapın, iyi yapın, sadece onu yapın](#tek-bir-işi-yapın-iyi-yapın-sadece-onu-yapın)
    - [Ya mutlu akışı (happy path) ya da hata yönetimini ele alın, ikisini birden değil](#ya-mutlu-akışı-happy-path-ya-da-hata-yönetimini-ele-alın-ikisini-birden-değil)
    - [Sadece bir soyutlama seviyesine inin](#sadece-bir-soyutlama-seviyesine-inin)
    - [Metotları kısa tutun](#metodları-kısa-tutun)
  - [Kontrol Akışı](#kontrol-akışı)
    - [Hızlı hata alın](#hızlı-hata-alın)
    - [CHECK vs. RETURN](#check-vs-return)
    - [CHECK ifadesini başka yerlerde kullanmaktan kaçının](#check-ifadesini-başka-yerlerde-kullanmaktan-kaçının)
- [Hata Yönetimi](#hata-yönetimi)
  - [Mesajlar](#mesajlar)
    - [Mesajları bulunması kolay olsun](#mesajların-bulunması-kolay-olsun)
  - [Dönüş Kodları](#dönüş-kodları)
    - [Dönüş kodları yerine istisnaları (exception) tercih edin](#dönüş-kodları-yerine-istisnaları-exception-tercih-edin)
    - [Hataların sessizce geçmesine izin vermeyin](#hataların-sessizce-geçmesine-izin-vermeyin)
  - [İstisnalar (Exceptions)](#istisnalar-exceptions)
    - [İstisnalar hatalar içindir, normal durumlar için değil](#istisnalar-hatalar-içindir-normal-durumlar-için-değil)
    - [Sınıf tabanlı (class-based) istisnalar kullanın](#sınıf-tabanlı-class-based-istisnalar-kullanın)
  - [Fırlatma (Throwing)](#fırlatma-throwing)
    - [Kendi üst sınıflarınızı (super class) kullanın](#kendi-üst-sınıflarınızı-super-class-kullanın)
    - [Tek bir türde istisna fırlatın](#tek-bir-türde-istisna-fırlatın)
    - [Çağıranların (callers) hata durumlarını ayırt edebilmesi için alt sınıflar (sub classes) kullanın](#çağıranların-callers-hata-durumlarını-ayırt-edebilmesi-için-alt-sınıflar-sub-classes-kullanın)
    - [Yönetilebilir istisnalar için CX_STATIC_CHECK fırlatın](#yönetilebilir-istisnalar-için-cx_static_check-fırlatın)
    - [Geri dönülemeyen durumlar için CX_NO_CHECK fırlatın](#geri-dönülemeyen-durumlar-için-cx_no_check-fırlatın)
    - [Önlenebilir istisnalar için CX_DYNAMIC_CHECK kullanmayı değerlendirin](#önlenebilir-istisnalar-için-cx_dynamic-check-kullanmayı-değerlendirin)
    - [Tamamen geri dönülemez durumlar için programı dump'a düşürün](#tamamen-geri-dönüleme-durumlar-için-programı-dumpa-düşürün)
    - [RAISE EXCEPTION TYPE yerine RAISE EXCEPTION NEW kullanmayı tercih edin](#raise-exception-type-yerine-raise-exception-new-kullanmayı-tercih-edin)
  - [Yakalama (Catching)](#yakalama-catching)
    - [Yabancı (dış) istisnaları sarmalayın, kodunuza sızmalarına izin vermeyin](#yabancı-dış-istisnaları-sarmalayın-kodunuza-sızmalarına-izin-vermeyin)
- [Yorumlar](#yorumlar)
  - [Kodla kendinizi ifade edin, yorumla değil](#kodla-kendinizi-ifade-edin-yorumla-değil)
  - [Kötü isimlendirme için yorumları bahane etmeyin](#kötü-isimlendirme-için-yorumları-bahane-etmeyin)
  - [Kodu bölmek için yorum yerine metod kullanın](#kodu-bölmek-için-yorum-yerine-metod-kullanın)
  - [Ne yaptığınızı değil, neden yaptığınızı açıklayın](#ne-yaptığınızı-değil-neden-yaptığınızı-açıklayın)
  - [Tasarım açıklamaları koda değil, tasarım dökümanlarına yazılır](#tasarım-açıklamaları-koda-değil-tasarım-dökümanlarına-yazılır)
  - [" ile yorum yazın, * ile yazmayın ](#ile-yorum-yazın-ile-yazmayın)
  - [Yorumları ilgili satırdan önce yazın](#yorumları-ilgili-satırdan-önce-yazın)
  - [Kodu yorum satırı yapmak yerine silin](#kodu-yorum-satırı-yapmak-yerine-silin)
  - [Manuel olarak versiyon kontrolü yapmayın](#manuel-olarak-versiyon-kontrolü-yapmayın)
  - [FIXME, TODO, ve XXX ekleyin ve kullanıcı ID'nizi ekleyin](#fixme-todo-ve-xxx-ekleyin-ve-kullanıcı-idnizi-ekleyin)
  - [Metot imzası ve bitiş yorumları eklemeyin](#metod-imzası-ve-bitiş-yorumları-eklemeyin)
  - [Mesaj metinlerini yorumla tekrar etmeyin](#mesaj-metinlerini-yorumla-tekrar-etmeyin)
  - [ABAP Doc sadece herkese açık API'lerde kullanılmalı](#abap-doc-sadece-herkese-açık-apilerde-kullanılmalı)
  - [Pseudo yorumlar yerine pragma kullanın](#pseudo-yorumlar-yerine-pragma-kullanın)
- [Biçimlendirme](#biçimlendirme)
  - [Tutarlı olun](#tutarlı-olun)
  - [Yazarken değil, okurken kolaylık sağlayın](#yazarken-değil-okurken-kolaylık-sağlayın)
  - [Aktifleştirmeden önce ABAP Biçimlendirici'yi kullanın](#aktifleştirmeden-önce-abap-biçimlendiriciyi-kullanın)
  - [Ekipteki ortak ABAP Biçimlendirici ayarlarını kullanın](#ekipteki-ortak-abap-biçimlendirici-ayarlarını-kullanın)
  - [Bir satırda yalnızca bir ifade olsun](#bir-satırda-yalnızca-bir-ifade-olsun)
  - [Satır uzunluğunu makul seviyede tutun](#satır-uzunluğunu-makul-seviyede-tutun)
  - [Kodunuzu sadeleştirin](#kodunuzu-sadeleştirin)
  - [Ayırmak için yalnızca tek satır boşluk bırakın, fazlasını değil](#ayırmak-için-yalnızca-tek-satır-boşluk-bırakın-fazlasını-değil)
  - [Boş satırları takıntı haline getirmeyin](#boş-satırları-takıntı-haline-getirmeyin)
  - [Aynı nesneye yapılan atamaları hizalayın, farklılara değil](#aynı-nesneye-yapılan-atamaları-hizalayın-farklılara-değil)
  - [Parantezleri satır sonunda kapatın](#parantezleri-satır-sonunda-kapatın)
  - [Tek parametreleri çağrılar aynı satırda kalmalı](#tek-parametreleri-çağrılar-aynı-satırda-kalmalı)
  - [Parametreler çağrıdan hemen sonra gelmeli](#parametreler-çağrıdan-hemen-sonra-gelmeli)
  - [Satır kırılıyorsa, parametreleri çağırın altına girintileyin](#satır-kırılıyorsa-parametreleri-çağırın-altına-girintileyin)
  - [Çok sayıda parametre varsa satır kırın](#çok-sayıda-parametre-varsa-satır-kırın)
  - [Parametreleri hizalayın](#parametreleri-hizalayın)
  - [Satır çok uzunsa çağrıyı yeni satıra kırın](#satır-çok-uzunsa-çağrıyı-yeni-satıra-kırın)
  - [Tab'la girintileyin ve hizalayın](#tabla-girintileyin-ve-hizalayın)
  - [İç içe tanımalamalar, metod çağrıları gibi girintilenmeli](#iç-içe-tanımalamalar-metod-çağrıları-gibi-girintilenmeli)
  - [Tip tanımlarında hizalama yapmayın](#tip-tanımlarında-hizalama-yapmayın)
  - [Atamaları zincirleme yapmayın](#atamaları-zincirleme-yapmayın)
- [Test Etme](#test-etme)
  - [İlkeler](#ilkeler)
    - [Test edilebilir kod yazın](#test-edilebilir-kod-yazın)
    - [Başkalarının sizi mock edebilmesini sağlayın](#başkalarının-sizi-mock-edebilmesini-sağlayın) 
    - [Okunabilirlik kuralları](#okunabilirlik-kuralları)
    - [Kopyalama yapmayın, test raporu yazmayın](#kopyalama-yapmayın-test-raporu-yazmayın)
    - [Private alanları değil public metodları test edin](#private-alanları-değil-public-metodları-test-edin)
    - [Kapsam (coverage) takıntısı yapmayın](#kapsam-coverage-takıntısı-yapmayın)
  - [Test Sınıfları](#test-sınıfları)
    - [Yerel test sınıflarını amacına göre adlandırın](#yerel-test-sınıflarını-amacına-göre-adlandırın)
    - [Testleri yerel sınıflarda tutun](#testleri-yerel-sınıflarda-tutun)
    - [Yardımcı metodları yardım sınıflarında tutun](#yardımcı-metodları-yardım-sınıflarında-tutun)
    - [Test sınıfları nasıl çalıştırılır](#test-sınıfları-nasıl-çalıştırılır)
  - [Test Edilen Kod](#test-edilen-kod)
    - [Test edilen kodu anlamlı şekilde adlandırın, olmazsa CUT kullanın](#test-edilen-kodu-anlamlı-şekilde-adlandırın-olmazsa-cut-kullanın)
    - [Uygulamaya değil, arayüze karşı test yazın](#uygulamaya-değil-arayüze-karşı-test-yazın)
    - [Test edilen kod çağrısını ayrı bir metoda çıkarın](#test-edilen-kod-çağrısını-ayrı-bir-metoda-çıkarın)
  - [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection)
    - [Test çiftlerini enjekte etmek için bağımlılık tersine çevrimini (Dependency Inversion) kullanın](#test-çiftlerini-enjekte-etmek-için-bağımlılık-tersine-çevrimini-dependency-inversion-kullanın)
    - [ABAP Test Double aracını kullanmayı düşünün](#abap-test-double-aracını-kullanmayı-düşünün)
    - [Test araçlarından faydalanın](#test-araçlarından-faydalanın)
    - [Test dikişlerini geçici çözüm olarak kullanın](#test-dikişlerini-geçici-çözüm-olarak-kullanın)
    - [Dependency-inverting constructor'a erişmek için LOCAL FRIENDS kullanın](#dependency-inverting-constructora-erişmek-için-local-friends-kullanın)
    - [LOCAL FRIENDS'i test edilen kodu ihlal etmek için kullanmayın](#local-friendsi-test-edilen-kodu-ihlal-etmek-için-kullanmayın)
    - [Sadece test sırasında kullanılacak özellikleri production koduna eklemeyin](#sadece-test-sırasında-kullanılacak-özellikleri-production-koduna-eklemeyin)
    - [Mock işlemleri için alt sınıf yaratmayın](#mock-işlemleri-için-alt-sınıf-yaratmayın)
    - [Gereksiz şeyleri mock'lamayın](#gereksiz-şeyleri-mocklamayın)
    - [Kendi test framework'ünüzü yazmayın](#kendi-test-frameworkünüzü-yazmayın)
  - [Test Metodları](#test-metodları)
    - [Test metodu isimleri: verilen ve bekleneni yansıtsın](#test-metodu-isimleri-verilen-ve-bekleneni-yansıtsın)
    - [Given-When-Then yapısını kullanın](#given-when-then-yapısını-kullanın)
    - ["When" tam olarak bir çağrı olmalıdır](#when-tam-olarak-bir-çağrı-olmalıdır)
    - [Gerçekten gerekmedikçe TEARDOWN kullanmayın](#gerçekten-gerekmedikçe-teardown-kullanmayın)
  - [Test Verisi (Test Data)](#test-verisi-test-data)
    - [Anlamı fark etmek kolay olmalı](#anlamı-fark-etmek-kolay-olmalı)
    - [Farklılıklar hemen göze çarpmalı](#farklılıklar-hemen-göze-çarpmalı)
    - [Test verisinin amacı ve önemini tanımlamak için sabitler (constant) kullanın](#test-verisinin-amacı-ve-önemini-tanımlamak-için-sabitler-constant-kullanın)
  - [Doğrulamalar (Assertions)](#doğrulamalar-assertions)
    - [Az ve odaklı doğrulamalar yapın](#az-ve-odaklı-doğrulamalar-yapın)
    - [Doğru ASSERT türünü kullanın](#doğru-assert-türünü-kullanın)
    - [Miktarı değil içeriği doğrulayın](#miktarı-değil-içeriği-doğrulayın)
    - [İçeriği değil kaliteyi doğrulayın](#içeriği-değil-kaliteyi-doğrulayın)
    - [Beklenen exception'lar için FAIL kullanın](#beklenen-exceptionlar-için-fail-kullanın)
    - [Beklenmeyen exception'ları yakalayıp FAIL demek yerine ilerletin](#beklenmeyen-exceptionları-yakalayıp-fail-demek-yerine-ilerletin)
    - [Kodunuzu kısaltmak ve tekrarları önlemek için özel ASSERT'lar yazın](#kodunuzu-kısaltmak-ve-tekrarları-önlemek-için-özel-assertlar-yazın)

## Nasıl Yapılır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#nasıl-yapılır)

### Clean Code yazmaya nasıl başlanır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Nasıl Yapılır](#nasıl-yapılır) > [Bu bölüm](#clean-code-yazmaya-nasıl-başlanır)

Clean Code konusunda yeniyseniz, önce [Robert C. Martin's _Clean Code_] okumalısınız.
[Clean Code Developer initiative](https://clean-code-developer.com/)
konuya genel olarak didaktik açıdan akıcı ve adım adım bir giriş yapmanıza yardımcı olabilir.

Kolayca anlaşılabilen ve genel olarak kabul gören şeylerle başlamanızı tavsiye ederiz,
Örneğin [Boolean Değerler](#boolean-değerler), [Koşullar (Conditions)](#koşullar-conditions), ve [If Yapıları](#if-yapıları).

Muhtemelen en çok [Metotlar](#metodlar) bölümünden, özellikle
 [Tek bir işi yapın, iyi yapın, sadece onu yapın](#tek-bir-işi-yapın-iyi-yapın-sadece-onu-yapın) ve [Metotları kısa tutun](#metodları-kısa-tutun) alt bölümlerinden fayda sağlayacaksınız; çünkü bunlar kodunuzun genel yapısını büyük ölçüde iyileştirir.

Buradaki bazı konular, işlerinde deneyimli fakat Clean Code konusunda yeni olan ekiplerde zorlu tartışmalara yol açabilir; bu konular tamamen "sağlıklı"dır, ancak başlangıçta insanlar kendilerini bu konulara alışmakta zorlanabilir.

Bu tartışmaya açık konulara daha sonra geçin;
özellikle [Yorumlar](#yorumlar), [İsimlendirme](#isimlendirme), ve [Biçimlendirme](#biçimlendirme)
neredeyse dini tartışmalara yol açabilir ve yalnızca Clean Code'un olumlu etkilerini bizzat görmüş ekipler tarafından ele alınmalıdır

### Eski (Legacy) kodu nasıl refaktör edersiniz

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Nasıl Yapılır](#nasıl-yapılır) > [Bu bölüm](#eski-legacy-kodu-nasıl-refaktör-edersiniz)

[Boolean Değerler](#boolean-değerler), [Koşullar (Conditions)](#koşullar-conditions), [If Yapıları](#if-yapıları),
ve [Metotlar](#metodlar) konuları, değiştiremeyeceğiniz ya da
değiştirmek istemediğiniz tonlarca koda sahip eski bir projede çalışıyorsanız en faydalı olanlardır;
çünkü bu konular çatışma yaratmadan yeni koda uygulanabilir.

[İsimlendirme](#isimlendirme) konusu, eski (legacy) projeler için oldukça zorludur.
Yeni ve eski kod arasında uyumsuzluk oluşmasına neden olabilir.
Bu yüzden, [Öneklerden kaçının, özellikle Macar notasyonu kullanmaktan](#öneklerden-kaçının-özellikle-macar-notasyonu-kullanmaktan) gibi
bölümler, bazı durumlarda göz ardı edilmesi gereken öneriler arasında yer alabilir.

Refaktoring yaparken, aynı geliştirme nesnesi içinde farklı geliştirme stillerini
karıştırmamaya çalışın. Eğer eski (legacy) kod yalnızca önceden yapılan (up-front)
değişken tanımlamalarını içeriyorsa ve tüm kodu satır için tanımlamalara dönüştürmek mümkün değilse,
iki stili karıştırmak yerine eski stilde kalmak muhtemelen daha iyi olacaktır.
Farklı stilleri bir arada kullanmanın kafa karışıklığına yol açabileceği birkaç benzer
durum daha vardır. Örneğin:

- Döngü sırasında `REF TO` ile `FIELD-SYMBOL` kullanımını karıştırmak.
- Bir `CONSTRUCTOR` çağrılırken `NEW` ve `CREATE OBJECT` ifadelerini birlikte kullanmak.
- Yalnızca tek bir parametre döndüren yöntemlerde, metod imzalarında `RETURNING` ve `EXPORTING` ifadelerini karıştırmak

Refaktoring için dört adımlı bir planla iyi sonuçlar elde ettiğimizi gözlemledik.

1. Ekibi sürece dahil edin. Yeni tarzı ekibinize anlatın ve açıklayın,
projedeki tüm ekip üyelerinin buna katılmasını sağlayın. Tüm yönergeleri
baştan benimsemek zorunda değilsiniz. Tartışmasız kabul görecek küçük bir alt küme
ile başlayın ve zamanla geliştirin.

1. Günlük iş rutininizde _izci kuralını_ uygulayın:
_Düzenlediğin kodu her zaman bulduğundan biraz daha temiz bırak._.
Bu konuda takıntılı davranıp saatlerce "kamp alanını temizlemeye" çalışmanıza gerek yok;
sadece birkaç dakika fazladan zaman ayırın ve zamanla bu küçük iyileştirmelerin nasıl
biriktiğini gözlemleyin.

1. _Temiz adacıklar_ oluşturun: Zaman zaman küçük bir nesne ya da bileşen seçin ve her yönüyle
temiz hale getirmeye çalışın. Bu adacıklar yaptığınız işin faydasını gösterir ve sonraki refaktoringler
için sağlam ve test edilmiş birer üs oluşturur.

1. Bunu konuşun. Eski usul [Fagan code incelemeleri](https://en.wikipedia.org/wiki/Fagan_inspection) yapın,
bilgilendirme oturumları düzenleyin ya da favori sohbet aracınızda tartışma kanalları açın deneyimlerinizi
ve öğrendiklerinizi paylaşmanız, ekibin ortak bir anlayış geliştirmesi açısından kritik önem taşır.

### Otomatik nasıl kontrol edersiniz

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Nasıl Yapılır](#nasıl-yapılır) > [Bu bölüm](otomatik-nasıl-kontrol-edersiniz)

[ABAP için code pal](https://github.com/SAP/code-pal-for-abap)
Clean ABAP için kapsamlı bir otomatik denetim paketi sunar.

ABAP Test Cockpit, Code Inspector, Extended Check, ve Checkman bazı
sorunları bulmanıza yardımcı olabilecek çeşitli kontroller sunar.

[abapOpenChecks](https://github.com/larshp/abapOpenChecks),
Code Inspector kontrollerinden oluşan açık kaynaklı bir koleksiyon
burada açıklanan bazı anti-pattern'leri de kapsamaktadır.

[abaplint](https://github.com/abaplint/abaplint), ABAP ayrıştırıcısının (parser) açık kaynaklı,
bir yeniden uygulamasıdır. SAP sistemi olmadan çalışır ve abapGit kullanılarak serileştirilmiş kod
üzerinde kullanılmak üzere tasarlanmıştır. GitHub Actions, Jenkins, metin düzenleyiciler gibi çeşitli
entegrasyonları destekler, bazı anti-pattern'leri tespit eder ve aynı zamanda biçimlendirme ile kodlama 
kurallarını denetlemek için de kullanılabilir.

### Diğer rehberlerle nasıl ilişkilendirilir

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Nasıl Yapılır](#nasıl-yapılır) > [Bu bölüm](#diğer-rehberlerle-nasıl-ilişkilendirilir)

Rehberimiz, Clean Code'un _ruhunu_ takip eder;
yani ABAP programlama dilini göre bazı uyarlamalar yaptık, örneğin
[Yönetilebilir istisnalar için CX_STATIC_CHECK fırlatın](#yönetilebilir-istisnalar-için-cx_static_check-fırlatın).

Bazı bilgiler, büyük ölçüde uyumlu olduğumuz
[ABAP Programlama Kılavuzu](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_pgl.htm)'ndan alınmıştır;
sapmalar belirtilmiş olup, her zaman daha temiz kod yazma ruhuna uygun bir şekilde yapılmıştır.

Bu kılavuz aynı zaman
[DSAG'ın ABAP Geliştirme Önerileri](https://dsag.de/wp-content/uploads/2021/12/dsag_recommendation_abap_development.pdf)'ne de saygı gösterir,
ancak çoğu detayda biz daha titiziz.

Yayınlanmasından bu yana, Clean ABAP birçok SAP iç geliştirme ekibi için
referans kılavuz haline gelmiştir; bunlar arasında S/4HANA üzerinde çalışan
birkaç yüz geliştirici de bulunmaktadır.

### Nasıl karşı çıkmalısınız

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Nasıl Yapılır](#nasıl-yapılır) > [Bu bölüm](#nasıl-karşı-çıkmalısınız)

Bu stil rehberini, Clean Code'a zaten aşina olan veya şu anda üzerinde çalışan
okuyucular için yazdık; özellikle Clean Code'un _ABAP'a özgü uygulanışına_ odaklandık.

Lütfen dikkat edin, bu yüzden orjinal kitap ve ilgili kaynaklardaki tüm kavramları
aynı uzunlukta ve derinlikte tanıtmadık. Ancak bu kaynaklar hala okumaya değerdir, özellikle
burada açıklamalarımız yetersiz kaldıysa ve bu yüzden katılmıyorsanız. Rehberimizin temelini daha iyi
anlamak için bölümlerdeki bağlantıları kullanarak arka plan bilgilerini okuyabilirsiniz.

Burada söylediklerimiz hakkında tartışmak ve katılmamakta özgürsünüz. Clean Code'un temel ilkelerinden
biri _ekibin karar vermesi_dir. Yine de, herhangi bir şeyi reddetmeden önce ona adil bir şans vermeyi unutmayın.


[CONTRIBUTING.md](../CONTRIBUTING.md) bu rehberi nasıl değiştirebileceğiniz veya küçük detaylarda ondan nasıl sapabileceğinizi önerir.

## İsimlendirme

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#isimlendirme)

### Açıklayıcı isimler kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#açıklayıcı-isimler-kullanın)

Anlam ve içeriği yansıtan isimler kullanın

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

Veri tipine veya teknik kodlamaya odaklanmayın.
Bunlar kodun anlaşılmasına pek katkı sağlamaz.

```ABAP
" anti-pattern
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[Kötü isimlendirmeyi yorumlarla düzeltmeye çalışmayın](#kötü-isimlendirme-için-yorumları-bahane-etmeyin)

> Daha fazlasını okuyun _Bölüm 2: Meaningful Names: Use Intention-Revealing Names_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Çözüm alanı ve problem alanı terimlerini tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#çözüm-alanı-ve-problem-alanı-terimlerini-tercih-edin)

İyi isimler ararken, çözüm alanında örneğin "kuyruk" veya "ağaç" gibi bilgisayar bilimi terimlerinde
ve problem alanında örneğin "hesap" veya "defter" gibi iş alanına ait terimlerde arama yapın.

İş odaklı katmanlar, problem alanına göre adlandırıldıklarında en iyi şekilde ifade edilir.
Bu özellikle, API'ler ve iş nesneleri gibi Domain-Driven Design (Alan Odaklı Tasarım) yaklaşımıyla
tasarlanmış bileşenler için geçerlidir.

Fabrika sınıfları ve soyut algoritmalar gibi çoğunlukla teknik işlevsellik sağlayan katmanlar,
çözüm alanına göre adlandırıldıklarında en iyi şekilde ifade edilir.

Her durumda, kendi dilinizi uydurmaya çalışmayın. Geliştiriciler, ürün sahipleri, iş ortakları ve
müşteriler arasında bilgi alışverişi yapabilmemiz gerekiyor, bu yüzden herkesin özel bir sözlüğe
ihtiyaç duymadan ilişki kurabileceği isimler seçin.

> Daha fazlasını Bölüm 2: Meaningful Names: Use Solution Domain Names_ and _[...]:
> Use Problem Domain Names_ [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Çoğul eklerini kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#çoğul-eklerini-kullanın)

SAP'de, şeylerin tabloları için tekil isimler kullanma gibi bir eski uygulama vardır.
Örneği `country` ülkeler tablosu gibi. Dış dünyada ise şeylerin listeleri için 
genellikle çoğul kullanma eğilimi yaygındır. Bu nedenle, bunun 
yerine `countries` (ülkeler) kullanılmasını tavsiye ediyoruz.

> Du tavsiye, özellikle değişkenler ve özelliker gibi şeyleri hedef almaktadır.
> Geliştirme nesneleri için ise, aynı şekilde mantıklı olan birbirine rakip desenler olabilir.
> Bunlardan biri, veritabanı tablolarını ("şeffaf tablo") tekil olarak adlandırma geleneği
> gibi yaygın bir kuraldır.

> Daha fazla bilgi için _Bölüm 2: Meaningful Names: Use Intention-Revealing Names_ of [Robert C. Martin's _Clean Code_] kısmını okuyabilirsiniz.

### Okunabilir isimler kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#okunabilir-isimler-kullanın)

Nesneler hakkında çok düşünür ve konuşuruz, bu yüzden telaffuz edinebilen isimler kullanın.
Örneğin, `detection_object_types` gibi anlaşılır bir ismi `dobjt` gibi gizemle bir isme tercih edin.

> Daha fazla bilgi için _Chapter 2: Meaningful Names: Use Pronounceable Names_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz

### snake_case kullanın
> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#snake_case-kullanın)

ABAP büyük/küçük harf duyarsızdır, bu yüzden `snake_case` kullanımını tutarlı bir şekilde takip etmenizi öneririz.

İsimler için bir karakter sınırı vardır, örneğin, metodlar için 30 karakter. Bir nesnenin maksimum uzunluğuna
ulaştığınızda, `flatcase` veya `UPPERCASE` kullanmaya başlamak yerine, kısaltmalar kullanmayı dikkatlice tercih edin (bkz. [Her yerde aynı kısaltmaları kullanın](#her-yerde-aynı-kısaltmaları-kullanın)).


```ABAP
" milisaniye cinsinden ölçülen maksimum yanıt süresini içeren bir değişken
DATA max_response_time_in_millisec TYPE i.
```

Üstteki örnek daha iyidir

```ABAP
" anti-pattern
DATA maxresponsetimeinmilliseconds TYPE i.
```

### Kısaltmalardan kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#kısaltmalardan-kaçının)

Eğer yeterli alanınız varsa, isimleri tam olarak yazın.
Sadece uzunluk sınırlamalarını aşarsanız kısaltma yapmaya başlayın.

Kısaltma yapmanız gerekirse, öncelikle _önemsiz_ kelimelerden başlayın.

Kısaltmalar, ilk bakışta verimli gibi görünebilir, ancak  hızla belirsizleşebilir.
Örneğin `cust` içindeki "cust" ne anlama geliyor? "customizing", "customer", veya "custom"?
Üçü de SAP uygulamalarında yaygın olarak kullanılır.

> Daha fazla bilgi için _Chapter 2: Meaningful Names: Make Meaningful Distinctions_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Her yerde aynı kısaltmaları kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#her-yerde-aynı-kısaltmaları-kullanın)

İnsanlar, ilgili kodu bulmak için anahtar kelimeler arayacaklardır.
Bunu desteklemek için aynı şeyi ifade eden terimler için aynı kısaltmayı kullanın.
Örneğin, "dot", "dotype" gibi karışık kısaltmalar kullanmak yerine, "detection object type" her zaman "dobjt" olarak kısaltılmalıdır "

> Daha fazla bilgi için _Chapter 2: Meaningful Names: Use Searchable Names_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Sınıflar için isim, Metodlar için fiil kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#sınıflar-için-isim-metodlar-için-fiil-kullanın)

Sınıfları, arayüzleri ve nesneleri adlandırırken isimler veya isim öbekleri kullanın

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

Metodları adlandırırken fiiler veya fiil öbekleri kullanın

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

Boolean metodlarını `is_` ve `has_` gibi fiilerle başlatmak, okunabilirliği artırır ve akıcı bir okuma sağlar:

```ABAP
IF is_empty( table ).
```

Fonksiyonları, metodlar gibi adlandırmanızı öneririz.

```ABAP
FUNCTION /clean/read_alerts
```

### "data", "info", "object" gibi belirsiz kelimelerden kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#data-info-object-gibi-belirsiz-kelimelerden-kaçının)

Gereksiz kelime kullanmaktan kaçının

```ABAP
account  " account_data yerine
alert    " alert_object yerine
```

ya da gerçekten değer katan belirli kelimelerle değiştirin

```ABAP
user_preferences          " user_info yerine
response_time_in_seconds  " response_time_variable yerine
```

> Daha fazla bilgi için _Chapter 2: Meaningful Names: Make Meaningful Distinctions_ of [Robert C. Martin's _Clean Code_]kısmına bakabilirsiniz

### Her konsept için bir kelime kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#her-konsept-için-bir-kelime-kullanın)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

Bir konsept için bir terim seçin ve ona sadık kalın. Eşanlamlıları
karıştırmayın. Eşanlamlı kelimeler, okuyucunun gerçek farkları bulmak için
zaman harcamasına neden olur

```ABAP
" anti-pattern
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> Daha fazla bilgi için _Chapter 2: Meaningful Names: Pick One Word per Concept_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz

### Desen (Pattern) adlarını yalnızca onları kastediyorsanız kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#desen-pattern-adlarını-yalnızca-onları-kastediyorsanız-kullanın)

Yazılım tasarım desenlerinin isimleri sınıflar ve arayüzler için kullanmaktan kaçının, eğer gerçekten o deseni uygulamıyorsanız. Örneğin, eğer gerçekten Factory tasarım desenini uygulamıyorsanız, 
sınıfınıza `file_factory` adını vermeyin. Yazılım tasarım desenleri, belirli bir problemi çözmek için belirli bir yolu
izleyen kalıplardır, bu yüzden isimlendirmede dikkatli olunmalıdır.

En yaygın desenler şunlardır:
[singleton](https://en.wikipedia.org/wiki/Singleton_pattern),
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern),
[facade](https://en.wikipedia.org/wiki/Facade_pattern),
[composite](https://en.wikipedia.org/wiki/Composite_pattern),
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern),
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern),
[observer](https://en.wikipedia.org/wiki/Observer_pattern), ve
[strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

> Daha fazla bilgi için _Chapter 2: Meaningful Names: Avoid Disinformation_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz

### Öneklerden kaçının, özellikle Macar notasyonu kullanmaktan

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#öneklerden-kaçının-özellikle-macar-notasyonu-kullanmaktan)

Gereksiz yere daha uzun olan yerine

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

tüm kodlama öneklerinden _tamamen_ kurtulmanızı öneririz.

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> [Avoid Encodings](sub-sections/AvoidEncodings.md)
> mantığını derinlemesine açıklar

### Yerleşik Fonksiyonları Gizlemekten Kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [İsimlendirme](#isimlendirme) > [Bu bölüm](#yerleşik-fonksiyonları-gizlemekten-kaçının)

Bir sınıfın içinde, eğer sınıfın metodları ile aynı adı taşıyan yerleşik bir fonksiyon varsa, fonksiyon her zaman metodlar tarafından gizlenir. Bu durum, fonksiyonun parametre
sayısı ve tipi ile metodun parametre sayısı ve tipi ne olursa olsun geçerlidir. Yerleşik fonksiyonlara örnek olarak `condense( )`, `lines( )`, `line_exists( )`, `strlen( )`, vb. verilebilir.

```ABAP
"anti-pattern
METHODS lines RETURNING VALUE(result) TYPE i.    
METHODS line_exists RETURNING VALUE(result) TYPE i.  
```

```ABAP
"anti-pattern 
CLASS-METHODS condense RETURNING VALUE(result) TYPE i.   
CLASS-METHODS strlen RETURNING VALUE(result) TYPE i.  
```

> Daha fazla bilgi için [Built-In Functions - Obscuring with Methods](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-us/abenbuilt_in_functions_syntax.htm).


## Dil

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#dil)

### Eski (Legacy) koda dikkat edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Dil](#dil) > [Bu bölüm](#eski-legacy-koda-dikkat-edin)

Eğer eski ABAP sürümleri için kod yazıyorsanız, bu kılavuzdaki tavsiyeleri dikkatle
değerlendirin. Aşağıdaki birçok öneri, eski ABAP sürümlerinde desteklenmeyebilecek nispeten
yeni sözdimi ve yapılar kullanmaktadır. Takip etmek istediğiniz yönergeleri, desteklemeniz
gereken en eski sürümde doğrulayın. Temiz Kod'u tamamen göz ardı etmeyin, kuralların büyük bir 
kısmı (örneğin isimlendirme, yorumlama) _herhangi_ bir ABAP sürümünde çalışacaktır.

### Performansa dikkat edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Dil](#dil) > [Bu bölüm](#performansa-dikkat-edin)

Eğer yüksek performanslı bileşenler için kod yazıyorsanız, bu kılavuzdaki tavsiyeleri dikkatle
değerlendirin. Temiz Kod'un bazı yönleri, işleri yavaşlatabilir (daha fazla metod çağrısı) veya
daha fazla bellek tüketebilir (daha fazla nesne). ABAP, bazı özel özelliklere sahiptir ve bu durumları
şiddetlendirebilir. Örneğin, bir metodu çağırırken veri türlerini karşılaştırır, bu nedenle tek bir büyük 
metodu birçok alt metoda ayırmak, kodu daha yavaş hale getirebilir.

Ancak, belirsiz korkulara dayalı olarak erken optimizasyon yapmamayı şiddetle tavsiye ederiz. Kuralların büyük
bir kısmı (örneğin isimlendirme, yorumlama) hiçbir olumsuz etki yaratmaz. Şeyleri temiz, nesne yönelimli bir şekilde
inşa etmeye çalışın. Eğer bir şey çok yavaşsa, bir performans ölçümü yapın. Ancak o zaman, seçilen kuralları
gözden çıkarmak için, veri temelli bir karar almanız gerekir.

Bazı ek düşünceler, kısmen
[Martin Fowler's _Refactoring_](https://martinfowler.com/books/refactoring.html) kitabının 2. Bölümünden alınmıştır:

Tipik bir uygulamada, çalışma zamanının çoğu kodun çok küçük bir kısmında harcanır. Kodun sadece %10'u,
çalışma zamanının %90'ına kadar katkıda bulunabilir ve özellikle ABAP'ta çalışma zamanının büyük bir 
kısmı veritabanı zamanına bağlı olabilir.

Bu nedenle,  _tüm_ kodu her zaman süper verimli hale getirmek için büyük çaba harcamak, kaynakların en iyi şekilde
kullanımı değildir. Performansı göz ardı etmeyi önermiyoruz, ancak ilk geliştirme aşamasında temiz ve iyi yapılandırılmış
koda daha fazla odaklanmayı ve ardından profil aracı kullanarak optimize edilmesi gereken kritik alanları tespit etmeyi öneriyoruz

Aslında, böyle bir yaklaşımın performans üzerinde net bir olumlu etkisi olacağını savunuruz çünkü bu, daha hedeflemiş bir 
optimizasyon çabasıdır ve performans darboğazlarını tespit etmek, iyi yapılandırılmış kodu yeniden yapılandırmak ve ayarlamak
daha kolay olacaktır.

### Nesne yönelimli programlamayı prosedürel programlamaya tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Dil](#dil) > [Bu bölüm](#nesne-yönelimli-programlamayı-prosedürel-programlamaya-tercih-edin)

Nesne yönelimli programlar (sınıflar, arayüzler) daha iyi bölümlenir ve prosedürel
koda (fonksiyonlar, programlar) göre daha kolay yeniden düzenlenip test edilebilir.
Her ne kadar belirli durumlarda prosedürel nesneler sağlamanız gerekse de (bir RFC 
için fonksiyon, bir işlem için program), bu nesneler esas işlevi sağlayan ilgili 
bir sınıfı çağırmaktan fazlasını yapmamalıdır.


```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

> Aradaki farkları daha detaylı incelemek için
> [Function Groups vs. Classes](sub-sections/FunctionGroupsVsClasses.md)

### Fonksiyonel yapıları, prosedürel dil yapılarına tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Dil](#dil) > [Bu bölüm](#fonksiyonel-yapıları-prosedürel-dil-yapılarına-tercih-edin)

Genellikle daha kısa olurlar ve modern programcılar için daha doğal gelirler.

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

Aşağıdaki detaylı kuralların birçoğu, bu genel tavsiyenin yalnızca belirli tekrarlarıdır.

### Eski dönem dil elemanlarından kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Dil](#dil) > [Bu bölüm](#eski-dönem-dil-elemanlarından-kaçının)

ABAP sürümünüzü yükseltirken,
kullanımdan kaldırılmış dil öğelerini kontrol ettiğinizden emin olun
ve bunları kullanmaktan kaçının

Örneğin, aşağıdaki ifadede uygulanan `@`-escaped "host" değişkenleri,
neyin bir program değişkeni neyin veritabanındaki bir sütun olduğunu
biraz daha net gösterir.

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

[obsolete unescaped form](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm) ile karşılaştırıldığında

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

Daha yeni alternatifler kodun okunabilirliğini artırma eğilimindedir,
ve modern programlama paradigmalarıyla tasarım çatışmalarını azaltır,
öyle ki bunlara geçmek kodunuzu otomatik olarak temizleyebilir.

Çalışmaya devam ederken, eski öğeler işlem hızı ve bellek tüketimi ,
açısından optimizasyonlardan yararlanamayabilir.

Modern dil öğeleri sayesinde SAP'nin eğitimlerinde artık öğretilmediği için
eski yapılara artık aşina olmayan genç ABAP'çıları daha kolay işe alabilirsiniz.

SAP NetWeaver belgeleri, eski dil öğelerini listeleyen kararlı bir bölüm
içerir, örneğin
[NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.54](https://help.sap.com/doc/abapdocu_754_index_htm/7.54/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.55](https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.56](https://help.sap.com/doc/abapdocu_756_index_htm/7.56/en-US/index.htm?file=abenabap_obsolete.htm),
[NW 7.57](https://help.sap.com/doc/abapdocu_757_index_htm/7.57/en-US/index.htm?file=abenabap_obsolete.htm).

### Tasarım desenlerini akıllıca kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Dil](#dil) > [Bu bölüm](#tasarım-desenlerini-akıllıca-kullanın)

Uygun oldukları ve fark edilir bir fayda sağladıkları yerlerde kullanın.
Sırf kullanmış olmak için her yere design pattern uygulamayın.

## Sabitler (Constants)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#sabitler-constants)

### Sihirli sayılar yerine sabit değerler kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Sabitler (Constants)](#sabitler-constants) > [Bu bölüm](#sihirli-sayılar-yerine-sabit-değerler-kullanın)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

daha okunabilirdir

```ABAP
" anti-pattern
IF abap_type = 'D'.
```

> Daha fazla bilgi için _Chapter 17: Smells and Heuristics: G25:
> Replace Magic Numbers with Named Constants_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Sabit değerlerin de açıklayıcı isimlere ihtiyacı vardır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Sabitler (Constants)](#sabitler-constants) > [Bu bölüm](#sabit-değerlerin-de-açıklayıcı-isimlere-ihtiyacı-vardır)

ABAP'ta her değişmezi sabitlere sarma yönünde tarihsel bir eğilim vardır; bu sabitler genellikle 
adlarıyla yalnızca içeriklerini ve hatta yalnızca türlerini tekrarlar:

```ABAP
" anti-pattern 
CONSTANTS: 
  c_01 TYPE spart VALUE '01',
  c_mmsta TYPE mmsta VALUE '90'.
```
Her iki varyantın da çok az faydası vardır. Okuyucu için bilgilendirici değildir ve 
değerin değişmesi gerekiyorsa, değeriyle adlandırılan bir sabitin de yeniden adlandırılması gerekir. 

Kodlanmış bir sabit kod içinde bildirilirse, içeriği değil anlamı açıklanmalıdır.
```ABAP
CONSTANTS status_inactive TYPE mmsta VALUE '90'.
```
Sabitin değeri zaten yeterince açıklayıcıysa, bu değerin tekrarlanması elbette kabul edilebilir:
```ABAP
CONSTANTS status_cancelled TYPE sww_wistat value 'CANCELLED'.
```

> Not: Bu bölüm [Açıklayıcı isimler kullanın](#açıklayıcı-isimler-kullanın)'ın sabitlere uygulanmış halidir.

### Sabit değer arayüzlerine karşı ENUM tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Sabitler (Constants)](#sabitler-constants) > [Bu bölüm](#sabit-değer-arayüzlerine-karşı-enum-tercih-edin)

ABAP'a özgü numaralandırmaları `ENUM` ile kullanın (>= 7.51 sürümlerinde mevcuttur)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM type,
             warning,
             error,
           END OF ENUM type.
ENDCLASS.
```

ilgisiz şeyleri karıştırmak yerine
veya insanları sabit koleksiyonlarının "implemented" olabileceğine
yanıltmak yerine:
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

> [Enumerations](sub-sections/Enumerations.md)
> alternatif numaralandırma modellerini (henüz `ENUM` desteği olmayan eski sürümler için de geçerlidir)
> açıklar ve bunların avantaj ve dezavantajlarını tartışır.
> Daha fazla bilgi için _Chapter 17: Smells and Heuristics: J3: Constants versus Enums_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### ENUM veya Enumeration deseni kullanmazsanız, sabit değerlerinizi gruplandırın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Sabitler (Constants)](#sabitler-constants) > [Bu bölüm](#enum-veya-enumeration-deseni-kullanmazsanız-sabit-değerlerinizi-gruplandırın)

Enumerations kullanamıyorsanız ve sabitleri esnek bir şekilde, 
örneğin bir arayüzde toplamanız gerekiyorsa, en azından bunları gruplandırın:

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

ilişkiyi daha açık hale getirir

```ABAP
" Anti-pattern
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

Grup ayrıca, örneğin giriş doğrulama için grup bazında erişim sağlar:

```ABAP
DO.
  ASSIGN message_severity-(sy-index) TO FIELD-SYMBOL(<constant>).
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

> Daha fazla bilgi için _Chapter 17: Smells and Heuristics: G27: Structure over Convention_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

## Değişkenler

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#değişkenler)

### Önceden tanımlanmış değişkenler yerine satır içi değişken kullanımını tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Değişkenler](#değişkenler) > [Bu bölüm](#önceden-tanımlanmış-değişkenler-yerine-satır-içi-değişken-kullanımını-tercih-edin)

Bu yönergeleri izlerseniz, yöntemleriniz o kadar kısa (3-5 statements) hale gelecektir ki
değişkenleri ilk geçtiği yerde inline olarak bildirmek daha doğal görünecektir

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

değişkenleri metodun başında ayrı bir `DATA` bölümü ile bildirmekten daha iyidir

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

> Daha fazla bilgi için _Chapter 5: Formatting: Vertical Distance: Variable Declarations_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Değişkenleri tanımlandıkları ifade (statement) bloğu dışında kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Değişkenler](#değişkenler) > [Bu bölüm](#değişkenleri-tanımlandıkları-ifade-statement-bloğu-dışında-kullanmayın)

```ABAP
" anti-pattern
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

Bir statement bloğunda (bir `IF` veya `LOOP` bloğunda olduğu gibi) bildirilen bir değişken, bu bloğun dışında onu takip eden kodda da kullanılabilir.
Bu durum okuyucular için kafa karıştırıcıdır, özellikle de yöntem uzunsa ve bildirim hemen fark edilmiyorsa.

Değişken, içinde bildirildiği statement bloğunun dışında gerekliyse, önceden bildirin:

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> Daha fazla bilgi için _Chapter 5: Formatting: Vertical Distance: Variable Declarations_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Değişken tanımlarını zincirlemeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Değişkenler](#değişkenler) > [Bu bölüm](#değişken-tanımlarını-zincirlemeyin)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO reader.
```

Chaining (Zincirleme), tanımlanan değişkenlerin mantıksal düzeyde birbiriyle ilişkili olduğunu ima eder.
Bunu tutarlı bir şekilde kullanmak için, zincirlenen tüm değişkenlerin birlikte ait olduğundan 
emin olmanız ve değişken eklemek için ek zincir grupları oluşturmanız gerekir.
Bu mümkün olsa da, genellikle çabaya değmez.

Chaining (Zincirleme) ayrıca, her satırın farklı görünmesine ve değiştirmenin
gereksiz yere iki nokta, nokta ve virgüllerle uğraşmayı gerektirmesine neden olarak
yeniden biçimlendirme ve refactoring işlemlerini gereksiz yere karmaşıklaştırır. Bu da çabaya değmez.

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO reader.
```

> Ayrıca bkz. [Tip tanımlarında hizalama yapmayın](#tip-tanımlarında-hizalama-yapmayın)  
> Eğer data declaration chaining kullanılıyorsa, o zaman birlikte ait olan değişken grupları için her biri ayrı bir zincir kullanın.

### Dinamik veri erişimi için field symbol kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Değişkenler](#değişkenler) > [Bu bölüm](#dinamik-veri-erişimi-için-field-symbol-kullanmayın)

ABAP Platform 2021 itibarıyla, generically typed değişkenlere erişim veya bir değişkenin bileşenlerine dinamik erişim sağlamak için field symbol kullanmanın gerekli olduğu neredeyse hiçbir alan kalmamıştır.

Bu nedenle, aşağıdakine benzer bir kullanım yerine:

```ABAP
" anti-pattern
ASSIGN dref->* TO <fs>.
result = <fs>.
```
bunu yazın
```ABAP
result = dref->*.
```

Daha fazla örnek ve field symbol kullanılarak yapılan dinamik ve generic erişimlerin daha modern sözdizimsel yapılarla nasıl değiştirileceğine dair detaylı açıklamalar için
[New kinds of ABAP expressions (SAP blog)](https://blogs.sap.com/2021/10/19/new-kinds-of-abap-expressions/) bağlantısına bakın.
### Döngüleriniz için doğru hedefleri seçin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Değişkenler](#değişkenler) > [Bu bölüm](#döngüleriniz-için-doğru-hedefleri-seçin)

Bir ABAP döngüsü için üç olası hedef vardır: Bir field symbol (`LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).`), bir reference variable (`LOOP AT table REFERENCE INTO DATA(line).`) veya düz bir data object (`LOOP AT table INTO DATA(line).`).
Bunların her birinin farklı kullanım amaçları vardır:

- Field symbols, üzerinde yineleme yapılan veriyi okumak veya değiştirmek istediğinizde kullanılır.
- Data references, bu referanslara döngü dışında erişmeniz gerektiğinde kullanılır, örneğin referansları input parametre olarak methodlara geçirmek veya döngü bittikten sonra veriye referansları saklamak için.
- Data objects, verinin kendisinin bir kopyasına ihtiyaç duyduğunuzda veya tablonun satır tipi zaten bir referans olduğunda kullanılır.

Data references kullanılarak da veri okunabilir veya değiştirilebilir.
Yani, field symbol’lerin loop hedefi olarak kullanıldığı neredeyse tüm durumlar, referanslar kullanılarak da gerçekleştirilebilir.

Nesne yönelimli ABAP’ın genel referans kullanım desenine tutarlılık sağlamak için, mümkün olduğunda loop hedefi olarak referans kullanmak isteyebilirsiniz.
Öte yandan, satır tipinin tüm değerine erişmek istediğinizde, data references kullanmak, field symbol kullanmaya kıyasla gereksiz ek dereferencing işlemleri getirir.
Aşağıdaki ile


```ABAP
LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
  obj->do_something( <line> ).
ENDLOOP.
```
karşılaştırın

```ABAP
LOOP AT table REFERENCE INTO DATA(line).
  obj->do_something( line->* ).
ENDLOOP.
```

Ayrıca, field symbol üzerinden veri erişimi, referans üzerinden veri erişimine göre biraz daha hızlıdır.
Bu fark yalnızca döngülerin programın çalışma süresinin önemli bir kısmını oluşturduğu durumlarda fark edilir ve genellikle önemli değildir; örneğin, çalışma süresinin çoğunu veritabanı işlemleri veya diğer giriş/çıkış işlemleri oluşturuyorsa.

Bu nedenlerle, belirli uygulama içerğine bağlı olarak iki olası tutarlı stil vardır:

- Eğer içerik genel olarak nesneler ve referanslar kullanıyorsa ve döngülerdeki küçük performans kayıpları genellikle önemli değilse, mümkün olduğunda döngü hedefi olarak field symbol yerine referans kullanın.
- Eğer içerik çoğunlukla düz veri üzerinde ve referanslar veya nesneler üzerinde değil, çok fazla manipülasyon yapıyorsa veya döngülerdeki küçük performans kayıpları genellikle önemliyse, döngülerde veriyi okumak ve değiştirmek için field symbol kullanın.

## Tablolar

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#tablolar)

### Uygun tablo türünü kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#uygun-tablo-türünü-kullanmayı-tercih-edin)

- Genellikle `HASHED` tabloları, **tek adımda doldurulan**, **hiç değiştirilmeden** kalan ve **anahtarı üzerinden sıkça okunan** **büyük tablolar** için kullanırsınız.
  Hash tabloların doğasında bulunan bellek ve işlem yükü, bunları yalnızca büyük miktarda veri ve çok sayıda okuma işlemi için değerli kılar.
  Tablonun içeriğindeki her değişiklik, hash'in yeniden hesaplanmasını gerektirir ve bu maliyetlidir; bu yüzden sık değiştirilen tablolar için kullanılmamalıdır.

- Genellikle `SORTED` tabloları, her zaman **sıralı tutulması gereken**, **parça parça doldurulan** veya **değiştirilmesi gereken**, ve **bir veya daha fazla tam veya kısmi anahtar üzerinden sıkça okunan** ya da **belirli bir sırada işlenmesi gereken** **büyük tablolar** için kullanırsınız.
  İçerik eklemek, değiştirmek veya kaldırmak, doğru ekleme noktasını bulmayı gerektirir fakat tablonun geri kalanının dizinini ayarlamak gerekmez.
  Sıralı tablolar, yalnızca çok sayıda okuma işlemi için değerini gösterir.

- **Küçük tablolar** ve indeksleme işleminin faydasından çok yük oluşturduğu durumlar için ve ayrıca, **"array"** olarak kullanmak istediğiniz, yani satırların sırasını umursamadığınız ya da tam olarak eklendikleri sırada işlemek istediğiniz durumlar için `STANDARD` tabloları kullanın.
  Ayrıca, tabloya farklı erişim türleri gerekiyorsa, örneğin indeksli erişim ve `SORT` ve `BINARY SEARCH` üzerinden sıralı erişim gerekiyorsa da kullanılır.

> Bunlar yalnızca genel kılavuzlardır.
> Daha fazla ayrıntı için ABAP Language Help içindeki [_Selection of Table Category_](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenitab_cat.htm) makalesine bakın.


### DEFAULT KEY kullanımından kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#default-key-kullanımından-kaçının)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

Varsayılan anahtarlar (`DEFAULT KEY`) genellikle yalnızca daha yeni fonksiyonel ifadelerin çalışmasını sağlamak için eklenir.
Anahtarların kendileri ise çoğu zaman gereksizdir ve kaynakları boşa harcar.
Hatta, sayısal veri tiplerini göz ardı ettiklerinden dolayı belirsiz hatalara bile yol açabilirler.
`SORT` ve `DELETE ADJACENT` ifadeleri, açık bir alan listesi verilmediğinde internal tablonun birincil anahtarını kullanır ve `DEFAULT KEY` kullanıldığında, örneğin anahtarın bir bileşeni olarak sayısal alanlar mevcutsa, bu çok beklenmedik sonuçlara yol açabilir. Özellikle de `READ TABLE ... BINARY` gibi işlemlerle birlikte kullanıldığında.

Bu yüzden, ya anahtar bileşenlerini açıkça belirtin:

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

ya da hiç anahtara ihtiyaç duymuyorsanız `EMPTY KEY` kullanın.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> [Horst Keller’in _Internal Tables with Empty Key_ bloguna](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/) bakınız
>
> **Dikkat:** `EMPTY KEY` (açık sıralama alanları olmadan) kullanılan internal tablolarda `SORT` işlemi hiç sıralama yapmaz,
> ancak anahtarın boşluğu statik olarak belirlenebilirse sözdizimi uyarıları verilir.


### APPEND TO yerine INSERT INTO TABLE kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#append-to-yerine-insert-into-table-kullanmayı-tercih-edin)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` tüm tablo ve anahtar tipleri ile çalışır,
bu da performans gereksinimleriniz değişirse tablo tipi ve anahtar tanımlarını yeniden düzenlemenizi kolaylaştırır.

`APPEND TO` ise yalnızca `STANDARD` tabloyu dizi benzeri bir şekilde kullanıyorsanız ve eklenen kaydın son satır olmasını vurgulamak istiyorsanız kullanılmalıdır.


### READ TABLE veya LOOP AT yerine LINE_EXISTS kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#read-table-veya-loop-at-yerine-line_exists-kullanmayı-tercih-edin)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

niyetini daha açık ve kısa ifade eder

```ABAP
" anti-pattern
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

veya hatta

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### LOOP AT yerine READ TABLE kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#loop-at-yerine-read-table-kullanmayı-tercih-edin)

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

niyetini daha açık ve kısa ifade eder

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

veya hatta

```ABAP
" anti-pattern
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### İç içe IF yapıları yerine LOOP AT WHERE kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#iç-içe-if-yapıları-yerine-loop-at-where-kullanmayı-tercih-edin)

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

niyetini daha açık ve kısa ifade eder

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Gereksiz tablo okumalarından kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Tablolar](#tablolar) > [Bu bölüm](#gereksiz-tablo-okumalarından-kaçının)

Bir satırın orada _olmasını bekliyorsanız_,
bir kez okuyun ve oluşan hataya göre tepki verin,


```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

Ana kontrol akışını çift okuma ile gereksiz 
yere karmaşıklaştırmak ve yavaşlatmak yerine

```ABAP
" anti-pattern
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = my_table[ key = input ].
```
> Performans artışının yanı sıra,
> bu, daha genel olan
> [Focus on the happy path or error handling, but not both](#focus-on-the-happy-path-or-error-handling-but-not-both) yaklaşımının özel bir çeşididir.

## String Değerler

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#string-değerler)

### Sabit değerleri tanımlarken ` kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [String Değerler](#string-değerler) > [Bu bölüm](#sabit-değerleri-tanımlarken-kullanın)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

`'` kullanımından kaçının, çünkü bu gereksiz bir tip dönüşümü ekler ve okuyucunun `CHAR` mı yoksa `STRING` ile mi çalıştığını anlamasını zorlaştırır:

```ABAP
" anti-pattern
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` genellikle uygundur, ancak `CONSTANTS` için kullanılamaz ve sabit bir değer belirtirken gereksiz yük ekler:

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### Metinleri birleştirmek için | kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [String Değerler](#string-değerler) > [Bu bölüm](#metinleri-birleştirmek-için-kullanın)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```
String template'ler, özellikle bir metin içinde birden fazla değişken gömülü olduğunda,
neyin literal, neyin değişken olduğunu daha iyi vurgular.

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Boolean Değerler

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#boolean-değerler)

### Boolean'ları bilinçli kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Boolean Değerler](#boolean-değerler) > [Bu bölüm](#booleanları-bilinçli-kullanın)

Booleanların doğal bir tercih gibi göründüğü durumlarla sık sık karşılaşırız.

```ABAP
" anti-pattern
is_archived = abap_true.
```

Bakış açısında bir değişiklik olana kadar,
bir enumeration seçmemiz gerektiği düşünülebilir.

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

Genel olarak, Booleanlar
şeylerin türlerini ayırt etmek için kötü bir tercihtir,
çünkü neredeyse her zaman
sadece birine ya da diğerine ait olmayan durumlarla karşılaşırsınız.


```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

[Split method instead of Boolean input parameter](#split-method-instead-of-boolean-input-parameter)
ayrıca neden Boolean parametreleri her zaman sorgulamanız gerektiğini açıklar.

### Boolean'lar için ABAP_BOOL kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Boolean Değerler](#boolean-değerler) > [Bu bölüm](#booleanlar-için-abap_bool-kullanın)

```ABAP
DATA has_entries TYPE abap_bool.
```

`char1` genel tipini kullanmayın.
Teknik olarak uyumlu olmasına rağmen, aslında bir Boolean değişkenle çalıştığımızı gizler.

Ayrıca diğer Boolean tiplerden de kaçının çünkü genellikle garip yan etkileri olur;
örneğin `boolean` tipi üçüncü bir değer olan "undefined"ı destekler ve bu ince programlama hatalarına yol açar.

Bazı durumlarda, örneğin DynPro alanları için veri sözlüğü öğesine ihtiyacınız olabilir.
`abap_bool` burada kullanılamaz çünkü `abap` tip havuzunda tanımlıdır, veri sözlüğünde değil.
Bu durumda `abap_boolean` kullanın.
Özel bir açıklamaya ihtiyacınız varsa kendi veri öğenizi oluşturun.

> ABAP, evrensel bir Boolean veri tipine sahip olmayan tek programlama dili olabilir.
> Ancak, böyle bir tipin olması zorunludur.
> Bu tavsiye, ABAP Programlama Kılavuzları’na dayanmaktadır.

### Karşılaştırmalar için ABAP_TRUE ve ABAP_FALSE kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Boolean Değerler](#boolean-değerler) > [Bu bölüm](#karşılaştırmalar-için-abap_true-ve-abap_false-kullanın)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

Karakter karşılıkları olan `'X'` ve `' '` ya da `space` kullanmayın;
bunlar ifadenin Boolean olduğunu anlamayı zorlaştırır:

```ABAP
" anti-pattern
has_entries = 'X'.
IF has_entries = space.
```

`INITIAL` ile karşılaştırmalardan kaçının bu, okuyucunun `abap_bool`'un varsayılan değerinin `abap_false` olduğunu hatırlamasını gerektirir:

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> ABAP, yerleşik olarak true ve false için "constants" içermeyen tek programlama dili olabilir.
> Ancak, bunların olması zorunludur.
> Bu tavsiye, ABAP Programlama Kılavuzları’na dayanmaktadır.

### Boolean değişken atamalarında XSDBOOL kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Boolean Değerler](#boolean-değerler) > [Bu bölüm](#boolean-değişken-atamalarında-xsdbool-kullanın)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

Eşdeğer `IF`-`THEN`-`ELSE` yapısı gereksiz yere çok daha uzundur:

```ABAP
" anti-pattern
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```
`xsdbool`, amacımız için en iyi yöntemdir; çünkü doğrudan `char1` üretir ve bu, bizim boolean tipimiz olan `abap_bool` ile en iyi uyumu sağlar.
Eşdeğer fonksiyonlar olan `boolc` ve `boolx` farklı tipler üretir ve gereksiz bir örtük tip dönüşümü ekler.

`xsdbool` isminin şanssız ve yanıltıcı olduğunu kabul ediyoruz;
sonuçta, "xsd" önekinin ima ettiği "XML Schema Definition" ile hiç ilgilenmiyoruz.

`xsdbool`'a alternatif olarak `COND` ternary formu düşünülebilir.
Sözdizimi sezgiseldir, ancak `THEN abap_true` kısmını gereksiz yere tekrar ettiği için biraz daha uzundur ve örtük varsayılan değer olan `abap_false` bilgisini gerektirir
bu nedenle sadece ikincil bir çözüm olarak öneriyoruz.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## Koşullar (Conditions)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#koşullar-conditions)

### Koşulları mümkün olduğunca olumlu yazmaya çalışın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Koşullar (Conditions)](#koşullar-conditions) > [Bu bölüm](#koşulları-mümkün-olduğunca-olumlu-yazmaya-çalışın)

```ABAP
IF has_entries = abap_true.
```

Karşılaştırma için, aynı ifadenin tersine çevrilerek ne kadar anlaşılması zor hale geldiğine bakın:

```ABAP
" anti-pattern
IF has_no_entries = abap_false.
```

Başlıkta geçen "try" ifadesi, bunu zorlamamanız gerektiği anlamına gelir;
yani [boş IF blokları](#boş-if-blokları-kullanmayın): gibi sonuçlar doğuracak kadar zorlamayın:

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> Daha fazla bilgi için _Chapter 17: Smells and Heuristics: G29: Avoid Negative Conditionals_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### NOT IS yerine IS NOT tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Koşullar (Conditions)](#koşullar-conditions) > [Bu bölüm](#not-is-yerine-is-not-tercih-edin)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

Olumsuzlaştırma (Negation) mantıksal olarak eşdeğerdir,
ancak anlamayı zorlaştıran bir "zihinsel geri dönüş" gerektirir.

```ABAP
" anti-pattern
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```

> [Koşulları mümkün olduğunca olumlu yazmaya çalışın](#koşulları-mümkün-olduğunca-olumlu-yazmaya-çalışın)
> kuralının daha spesifik bir çeşididir.
> Ayrıca, ABAP programlama kılavuzlarındaki
> [Alternative Language Constructs](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenalternative_langu_guidl.htm)
> bölümünde de açıklandığı gibidir.


### Boolean metodlar için kestirimsel (predicative) çağrılar kullanmayı düşünün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Koşullar (Conditions)](#koşullar-conditions) > [Bu bölüm](#boolean-metodlar-için-kestirimsel-predicative-çağrılar-kullanmayı-düşünün)

Boolean metodlar için predicative (tahmini) method çağrısı, örneğin

```ABAP
IF [ NOT ] condition_is_fulfilled( ).
```

sadece çok kompakt olmakla kalmaz, aynı zamanda karşılaştırma ifadesi olarak kodu doğal dile daha yakın tutmayı sağlar.

```ABAP
" anti-pattern
IF condition_is_fulfilled( ) = abap_true / abap_false.
```

Unutmayın ki, predikatif method çağrısı `... meth( ) ...` aslında `... meth( ) IS NOT INITIAL ...` ifadesinin kısa halidir; detaylar için ABAP 
Keyword Documentation'daki [Predicative Method Call](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenpredicative_method_calls.htm) bölümüne bakabilirsiniz.
Bu yüzden kısa form sadece, dönen tipte non-initial değerin "true", initial değerin ise "false" anlamına geldiği metodlar için kullanılmalıdır.

### Karmaşık koşulları ayrıştırın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Koşullar (Conditions)](#koşullar-conditions) > [Bu bölüm](#karmaşık-koşulları-ayrıştırın)

Koşullar, onları oluşturan temel parçalara ayrıldığında daha kolay anlaşılır hale gelir:

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

Her şeyi olduğu yerde bırakmak yerine:

```ABAP
" anti-pattern
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> Yukarıda gösterildiği gibi koşulları hızlıca çıkarmak ve değişkenler oluşturmak için ABAP Development Tools hızlı düzeltmelerini kullanın.

### Karmaşık koşulları ayıklayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Koşullar (Conditions)](#koşullar-conditions) > [Bu bölüm](#karmaşık-koşulları-ayıklayın)

Karmaşık koşulları kendi metodlarına çıkarmak neredeyse her zaman iyi bir fikirdir:

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

## If Yapıları

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#if-yapıları)

### Boş IF blokları kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [If Yapıları](#if-yapıları) > [Bu bölüm](#boş-if-blokları-kullanmayın)

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

şundan daha kısa ve açıktır

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " do some magic
ENDIF.
```

### Çoklu alternatif durumlar için ELSE IF yerine CASE tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [If Yapıları](#if-yapıları) > [Bu bölüm](#çoklu-alternatif-durumlar-için-else-if-yerine-case-tercih-edin)

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

`CASE`, birbirini dışlayan alternatifler setini görmek için kolaylık sağlar.
Bir dizi `IF` ifadesinden daha hızlı olabilir, çünkü arka planda art arda değerlendirilen koşullar yerine farklı bir mikroişlemci komutuna dönüşebilir.
Ayrıca, ayırt edici değişkeni tekrar tekrar yazmak zorunda kalmadan yeni durumlar hızlıca ekleyebilirsiniz.
Bu ifade, yanlışlıkla iç içe `IF`-`ELSEIF` yapıları oluşturulduğunda ortaya çıkabilecek bazı hataları da önler.

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

### İç içe IF yapılarının derinliğini azaltın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [If Yapıları](#if-yapıları) > [Bu bölüm](#iç-içe-if-yapılarının-derinliğini-azaltın)

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

İç içe `IF` yapıları çok hızlı karmaşıklaşır ve tam kapsam için üssel sayıda test vakası gerektirir.

Karar ağaçları genellikle alt metodlar oluşturularak ve Boolean yardımcı değişkenler tanımlanarak parçalara ayrılabilir.

Diğer durumlar ise `IF` ifadelerinin birleştirilmesiyle sadeleştirilebilir, örneğin:

```ABAP
IF <this> AND <that>.
```

gereksiz yere iç içe geçmiş yapılar yerine

```ABAP
" anti-pattern
IF <this>.
  IF <that>.
```

## Düzenli İfadeler (Regex)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#düzenli-ifadeler-regex)

### Düzenli ifadeler yerine daha basit yöntemleri tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Düzenli İfadeler (Regex)](#düzenli-ifadeler-regex) > [Bu bölüm](#düzenli-ifadeler-yerine-daha-basit-yöntemleri-tercih-edin)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

Düzenli ifadeler (regular expressions) çok hızlı bir şekilde anlaşılması zor hale gelir.
Basit durumlar genellikle onlarsız daha kolaydır.

Düzenli ifadeler ayrıca genellikle daha fazla bellek ve işlem süresi tüketir,
çünkü çalışma zamanında bir ifade ağacına ayrıştırılıp yürütülebilir bir eşleyiciye derlenmeleri gerekir.
Basit çözümler ise doğrudan bir döngü ve geçici bir değişkenle halledilebilir.

### Regex yerine temel kontrolleri (basis checks) tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Düzenli İfadeler (Regex)](#düzenli-ifadeler-regex) > [Bu bölüm](#regex-yerine-temel-kontrolleri-basis-checks-tercih-edin)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

zaten var olan şeyleri yeniden icat etmek yerine

```ABAP
" anti-pattern
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> Düzenli ifadeler kullanılırken
> Don't-Repeat-Yourself (DRY) prensibine karşı doğal bir körlük eğilimi olduğu görülmektedir,
> karşılaştırmak için [Robert C. Martin’in _Clean Code_ kitabındaki _Chapter 17: Smells and Heuristics: General: G5: Duplication_ bölümüne](https://www.goodreads.com/book/show/3735293-clean-code) bakabilirsiniz.


### Karmaşık düzenli ifadeleri oluştururken parça parça birleştirmeyi düşünün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Düzenli İfadeler (Regex)](#düzenli-ifadeler-regex) > [Bu bölüm](#karmaşık-düzenli-ifadeleri-oluştururken-parça-parça-birleştirmeyi-düşünün)

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

Bazı karmaşık düzenli ifadeler, okuyucuya daha temel parçalardan nasıl oluşturuldukları gösterildiğinde daha anlaşılır hale gelir.

## Sınıflar

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#sınıflar)

### Sınıflar: Nesne Yönelimli yapı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Bu bölüm](#sınıflar-nesne-yönelimli-yapı)

#### Statik sınıflar yerine nesneleri tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Sınıflar: Nesne Yönelimli yapı](#sınıflar-nesne-yönelimli-yapı) > [Bu bölüm](#statik-sınıflar-yerine-nesneleri-tercih-edin)

Statik sınıflar, nesne yöneliminden elde edilen tüm avantajlardan vazgeçer.
Özellikle birim testlerde bağımlılıkları test double’larla değiştirmeyi neredeyse imkansız hale getirirler.

Bir sınıfı veya metodu statik yapıp yapmamayı düşünüyorsanız, cevap neredeyse her zaman: hayır olacaktır.

Bu kurala kabul edilmiş bir istisna ise, sade tip yardımcı (type utils) sınıflardır.
Bu sınıfların metodları, belirli ABAP tipleriyle etkileşimi kolaylaştırır.
Tamamen durumsuz (stateless) olmalarının yanı sıra, o kadar temel ve basittirler ki, sanki ABAP ifadeleri ya da yerleşik fonksiyonlar gibidirler.
Ayırıcı faktör, tüketicilerinin bu sınıfları kodlarına o kadar sıkı bağlamalarıdır ki, birim testlerde onları taklit etmek istemezler.

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

#### Kalıtım (inheritance) yerine bileşimi (composition) tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Sınıflar: Nesne Yönelimli yapı](#sınıflar-nesne-yönelimli-yapı) > [Bu bölüm](#kalıtım-inheritance-yerine-bileşimi-tercih-edin)

Sınıflar arasında kalıtım hiyerarşileri kurmaktan kaçının. Bunun yerine, kompozisyonu tercih edin.

Clean inheritance tasarımı zordur çünkü
[Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle) gibi kurallara uymanız gerekir.
Ayrıca, insanlar hiyerarşinin arkasındaki temel prensipleri anlamalı ve özümsemelidir; bu da anlaşılmasını zorlaştırır.
Kalıtım, metodların genellikle sadece alt sınıflarda erişilebilir olması nedeniyle yeniden kullanımı azaltır.
Ayrıca, üyeleri taşımak veya değiştirmek tüm hiyerarşi ağacında değişiklikler gerektirebileceğinden refactoring’i zorlaştırır.

Kompozisyon ise her biri belirli bir amaca hizmet eden küçük, bağımsız nesneler tasarlamaktır.
Bu nesneler basit delegasyon ve facade pattern’leri ile daha karmaşık nesnelere yeniden birleştirilebilir.
Kompozisyon daha fazla sınıf üretebilir, ancak başka dezavantajı yoktur.

Bu kural, doğru yerlerde kalıtım kullanmanızı engellemesin.
Kalıtım için iyi uygulamalar vardır,
örneğin [Composite design pattern](https://en.wikipedia.org/wiki/Composite_pattern).
Sadece kendinize, durumunuzda kalıtımın gerçekten dezavantajlardan daha fazla fayda sağlayıp sağlamayacağını sorgulayın.
Emin değilseniz, genellikle kompozisyon daha güvenli tercihtir.


> [Interfaces vs. abstract classes](sub-sections/InterfacesVsAbstractClasses.md)
bazı detayları karşılaştırır.


#### Aynı sınıf içerisinde durum tutan (stateful) ve durumsuz (stateless) yapıları karıştırmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Sınıflar: Nesne Yönelimli yapı](#sınıflar-nesne-yönelimli-yapı) > [Bu bölüm](#aynı-sınıf-içerisinde-durum-tutan-stateful-ve-durumsuz-stateless-yapıları-karıştırmayın)

Durumsuz (stateless) ve durumlu (stateful)
programlama paradigmalarını aynı sınıf içinde karıştırmayın.

Durumsuz programlamada, metodlar girdi alır ve çıktı üretir,
_hiçbir yan etki olmadan_,
böylece metodlar ne zaman ve hangi sırayla çağrılırsa çağrılsın aynı sonucu üretir.

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

Durumlu programlamada ise, nesnelerin iç durumunu metodları aracılığıyla değiştiririz,
yani bu programlama türü _yan etkilerle doludur_.


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

Her iki paradigma da uygundur ve kendi uygulama alanları vardır.
Ancak, aynı nesne içinde _karıştırmak_,
anlaşılması zor ve belirsiz taşma(carry-over) hataları ile eşzamanlılık problemleri yaratacak kodlar üretir.
Bunu yapmayın.


### Kapsam (Scope)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Bu bölüm](#kapsam-scope)

#### Varsayılan olarak global tanımlayın, sadece gerektiğinde lokal kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Kapsam (Scope)](#kapsam-scope) > [Bu bölüm](#varsaylın-olarak-global-tanımlayın-sadece-gerektiğinde-lokal-kullanın)

Global sınıflarla çalışmayı varsayılan tercih edin.
Local sınıfları ise yalnızca uygun olduğunda kullanın.

> Global sınıflar, veri sözlüğünde görünen sınıflardır.
> Local sınıflar ise başka bir geliştirme objesinin include’ı içinde yer alır
> ve yalnızca o objeye görünürdür.

Local sınıflar şu durumlar için uygundur:

- Çok spesifik özel veri yapıları için, 
örneğin global sınıfın verisi için sadece burada ihtiyaç duyulacak bir yineleyici (iterator),

- Karmaşık özel algoritmaların çıkarılması için,
örneğin sınıfınızın kodundan o özel amaçlı çoklu metodlu sort-aggregate algoritmasını ayırmak için,

- Global sınıfın belirli yönlerinin mocklanabilmesini sağlamak için,
örneğin tüm veritabanı erişimini ayrı bir local sınıfa çıkarıp, birim testlerde test double ile değiştirebilmek için.

Local sınıflar yeniden kullanımı zorlaştırır çünkü başka yerde kullanılamazlar.
Kolay çıkarılabilmelerine rağmen, insanlar genellikle onları bulamaz, bu da istenmeyen kod tekrarına yol açar.
Çok uzun local class include’larında yön bulmak, gezinmek ve hata ayıklamak zahmetli ve can sıkıcıdır.
ABAP, include seviyesinde kilitleme yaptığı için, insanlar local include’ın farklı parçalarında aynı anda çalışamazlar
(ayrı global sınıflar olsaydı mümkün olabilirdi).

Local sınıfların kullanımını yeniden gözden geçirin eğer:

- Local include’ınız onlarca sınıf ve binlerce satır kod içeriyorsa,
- Global sınıfları içinde başka sınıflar barındıran “package” olarak düşünüyorsanız,
- Global sınıflarınız boş gövdeli yapıya dönüşmüşse,
- Ayrı local include’larda kod tekrarları varsa,
- Geliştiriciler birbirlerini kilitleyip paralel çalışamaz hale geliyorsa,
- Takımlar birbirlerinin local alt ağaçlarını anlayamadığı için backlog tahminleriniz çok yükseliyorsa.

#### Eğer sınıf kalıtım (inheritance) için tasarlanmadıysa FINAL olarak işaretleyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Kapsam (Scope)](#kapsam-scope) > [Bu bölüm](#eğer-sınıf-kalıtım-inheritance-için-tasarlanmadıysa-final-olarak-işaretleyin)

Kalıtım için açıkça tasarlanmamış sınıfları `FINAL` yapın.

Sınıfların iş birliği tasarımında, ilk tercihiniz [kalıtım (inheritance) değil bileşim (composition) olmalıdır](#kalıtım-inheritance-yerine-bileşimi-tercih-edin).
Kalıtımı etkinleştirmek hafife alınacak bir şey değildir,
çünkü `PROTECTED` ile `PRIVATE` arasındaki farklar ve
[Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle) gibi kavramları düşünmenizi gerektirir,
aynı zamanda birçok tasarım iç detayını da donuklaştırır.
Eğer sınıf tasarımınızda bu konuları dikkate almadıysanız,
sınıfınızı yanlışlıkla kalıtılması önlemek için `FINAL` yapmalısınız.

Elbette kalıtım için bazı iyi uygulamalar vardır,
örneğin [composite tasarım deseni](https://en.wikipedia.org/wiki/Composite_pattern).
Business Add-Ins de alt sınıf oluşturulmasına izin vererek,
müşterinin orijinal kodun çoğunu yeniden kullanmasını kolaylaştırabilir.
Ancak, bu durumların tümü baştan itibaren kalıtım içerir.

[Arayüzleri implement etmeyen](#herkese-açık-public-örnek-instance-metodlar-bir-arayüzün-interface-parçası-olmalıdır) kirli (unclean) sınıflar ise,
birim testlerinde tüketicilerin bunları mock’layabilmesi için `FINAL` yapılmamalıdır.


#### Sınıf üyelerini varsayılan olarak PRIVATE yapın, sadece gerektiğinde PROTECTED kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Kapsam (Scope)](#kapsam-scope) > [Bu bölüm](#sınıf-üyelerini-varsayılan-olarak-private-yapın-sadece-gerektiğinde-protected-kullanın)

Varsayılan olarak attribute’ları, metodları ve diğer sınıf üyelerini `PRIVATE` yapın.

Yalnızca alt sınıfların bunları geçersiz kılmasını istiyorsanız `PROTECTED` yapın.

Sınıfların iç yapıları, yalnızca bilmesi gerekenlere açılmalıdır.
Bu sadece dışarıdan çağıranları değil, alt sınıfları da kapsar.
Bilginin gereğinden fazla erişilebilir olması, beklenmedik yeniden tanımlamalarla ince hatalara yol açabilir ve refactoring’i zorlaştırır,
çünkü dışarıdakiler değiştirilmesi gereken üyeleri sabitlemiş olur.

#### Getter yerine değiştirilemez (immutable) veri kullanmayı düşünün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Kapsam (Scope)](#kapsam-scope) > [Bu bölüm](#getter-yerine-değiştirilemez-immutable-veri-kullanmayı-düşünün)

Immutable, oluşturulduktan sonra asla değişmeyen bir nesnedir.
Bu tür nesneler için getter metodlar yerine public, sadece okunabilir attribute kullanmayı düşünün.

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

yerine

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

> **Dikkat:** Değeri değişen nesneler için public sadece-okunur attribute kullanmayın.
> Aksi takdirde, bu attribute’lar diğer kodlar tarafından gereksinim duyulup
> duyulmadığına bakılmaksızın her zaman güncel tutulmak zorunda kalır.


#### READ-ONLY ifadesini dikkatli kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Kapsam (Scope)](#kapsam-scope) > [Bu bölüm](#read-only-ifadesini-dikkatli-kullanın)

Birçok modern programlama dili, özellikle Java, sınıf üyelerini uygun yerlerde yalnızca okunur yapmayı önerir,
kazara yan etkilerin önüne geçmek için.

ABAP’ta veri tanımlamalarında `READ-ONLY` eklemesi bulunmasına rağmen, bunu ölçülü kullanmanızı tavsiye ederiz.

Öncelikle, bu ekleme yalnızca `PUBLIC SECTION` içinde kullanılabilir,
bu da uygulanabilirliğini ciddi şekilde sınırlar.
`PROTECTED` veya `PRIVATE` üyelere veya metod içi yerel değişkenlere eklenemez.

İkinci olarak, bu ekleme diğer programlama dillerinde beklenen davranıştan hafifçe farklıdır:
`READ-ONLY` veriler, sınıfın kendisi, arkadaşları ve alt sınıflarındaki herhangi bir metod tarafından yine serbestçe değiştirilebilir.
Bu, diğer dillerde yaygın olan "bir kez yaz, bir daha değiştirme" davranışıyla çelişir.
Bu fark kötü sürprizlere yol açabilir.

> Yanlış anlamaları önlemek için: Değişkenleri kazara değiştirmeye karşı korumak iyi bir uygulamadır.
> Uygun bir ifade olsaydı ABAP’ta da bunu uygulamanızı önerirdik.

### Yapılandırıcı (Constructors)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Bu bölüm](#yapılandırıcı-constructors)

#### CREATE OBJECT yerine NEW kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Yapılandırıcı (Constructors)](#yapılandırıcı-constructors) > [Bu bölüm](#create-object-yerine-new-kullanmayı-tercih-edin)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

gereksiz yere daha uzun olan yerine

```ABAP
" anti-pattern
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

elbette, dinamik tiplere ihtiyaç duyduğunuz durumlar hariç

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### Global sınıfınız CREATE PRIVATE ise, CONSTRUCTOR metodunu PUBLIC bırakın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Yapılandırıcı (Constructors)](#yapılandırıcı-constructors) > [Bu bölüm](#global-sınıfınız-create-private-ise-constructor-metodunu-public-bırakın)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

Bunun kendi içinde çelişkili olduğunu kabul ediyoruz.
Ancak, ABAP Yardımı’ndaki
[_Instance Constructor_](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abeninstance_constructor_guidl.htm) makalesine göre,
`CONSTRUCTOR`'ın `PUBLIC SECTION` içinde belirtilmesi, doğru derleme ve sözdizimi doğrulamasını garanti etmek için zorunludur.

Bu yalnızca global sınıflar için geçerlidir.
Local sınıflarda, constructor'ı olması gerektiği gibi private yapın.

#### Opsiyonel parametreler yerine birden fazla statik oluşturucu (creation) metodu kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Yapılandırıcı (Constructors)](#yapılandırıcı-constructors) > [Bu bölüm](#opsiyonel-parametreler-yerine-birden-fazla-statik-oluşturucu-creation-metodu-kullanmayı-tercih-edin)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP, [overloading](https://en.wikipedia.org/wiki/Function_overloading) desteği sunmaz.
İstenen anlamı sağlamak için optional parametreler yerine isim varyasyonları kullanın.

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

Genel kılavuz
[_Split methods instead of adding OPTIONAL parameters_](#split-methods-instead-of-adding-optional-parameters)
bunun arkasındaki mantığı açıklar.

Karmaşık yapıları,
[Builder design pattern](https://en.wikipedia.org/wiki/Builder_pattern) ile çok adımlı bir yapıya dönüştürmeyi düşünün.

#### Birden fazla oluşturucu metod varsa açıklayıcı isimler verin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Yapılandırıcı (Constructors)](#yapılandırıcı-constructors) > [Bu bölüm](#birden-fazla-oluşturucu-metod-varsa-açıklayıcı-isimler-verin)

Oluşturma metodlarına başlamak için iyi kelimeler `new_`, `create_` ve `construct_`’tur.
İnsanlar bunları sezgisel olarak nesne oluşturmayla ilişkilendirir.
Ayrıca, `new_from_template`, `create_as_copy` veya `create_by_name` gibi fiil öbeklerine güzelce uyum sağlarlar.

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

Bunun gibi anlamsız bir şey yerine

```ABAP
" anti-pattern
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### Sadece birden fazla örnek anlamsızsa singleton sınıf oluşturun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Classes](#classes) > [Yapılandırıcı (Constructors)](#yapılandırıcı-constructors) > [Bu bölüm](#sadece-birden-fazla-örnek-anlamsızsa-singleton-sınıf-oluşturun)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

Nesne yönelimli tasarımınız, bir nesnenin ikinci bir örneğinin anlamlı olmadığı durumlarda singleton pattern’i uygulayın.
Bu, her kullanıcının aynı nesneyle, aynı durum ve verilerle çalışmasını garanti eder.

Singleton pattern’i alışkanlıkla ya da performans kuralı diye kullanmayın.
En fazla yanlış kullanılan ve abartılan tasarım desenidir;
beklenmedik çapraz etkiler yaratır ve testleri gereksiz yere karmaşıklaştırır.

Birim nesne için tasarım kaynaklı bir sebep yoksa, bu kararı kullanıcıya bırakın.
Kullanıcı, constructor dışındaki yöntemlerle (örneğin factory ile) aynı sonuca ulaşabilir.

## Metotlar 

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#metodlar)

Bu kurallar, sınıflardaki metodlar ve function module’ler için geçerlidir.

### Çağrılar (Calls)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#çağrılar-calls)

#### Statik metodları nesne değişkenleri (instance variables) üzerinden çağırmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#statik-metodları-nesne-değişkenleri-instance-variables-üzerinden-çağırmayın)

Static metod çağırmak için şu yapıyı kullanın:
```ABAP
cl_my_class=>static_method( ).
```

`cl_my_class` sınıfına örnek değişkeni üzerinden çağırmak yerine
```ABAP
" anti-pattern
lo_my_instance->static_method( ).
```

Statik metod, doğrudan sınıfa bağlıdır ve bir örnek değişken üzerinden çağrılması kafa karıştırıcı olabilir.

Aynı sınıfın statik metodu, başka bir statik metod içindeyse, nitelendirmeden çağrılması kabul edilebilir.

```ABAP
METHOD static_method.
  another_static_method( ).
  yet_another( ).
ENDMETHOD.
```

Ancak, bir örnek metodun içinde bile, aynı sınıfın statik metodunu çağırırken, çağrıyı yine sınıf adıyla nitelendirmelisiniz:

```ABAP
CLASS cl_my_class IMPLEMENTATION.

  METHOD instance_method.
    cl_my_class=>a_static_method( ).
    another_instance_method( ).
  ENDMETHOD.

  ...
```

#### Tip tanımlarına nesne değişkenleri (instance variables) üzerinden çağırmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#dont-access-types-through-instance-variables)

Bir sınıf veya interface içinde tanımlı bir veri tipi kullanırken, bu tipi sınıfın/interface’in kendisi üzerinden erişin;
sınıf/interface örneği üzerinden değil.

```ABAP
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES foo TYPE i.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

INTERFACE lif.
  TYPES blah TYPE lcl=>foo.  
ENDINTERFACE.
```

Veri tipi için örneği kullanmak kafa karıştırıcı olur;
çünkü bu, tipin örneğe özgü olduğu izlenimini verir.

```ABAP
" anti-pattern
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES foo TYPE i.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

INTERFACE lif.
  DATA(ref) = new lcl( ).
  TYPES blah TYPE ref->foo.
ENDINTERFACE.
```

#### İşlevsel (functional) çağrıları prosedürel (procedural) çağrılara tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#işlevsel-functional-çağrıları-prosedürel-procedural-çağrılara-tercih-edin)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

gereksiz yere daha uzun olan yerine

```ABAP
" anti-pattern
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Dinamik tip kullanımı fonksiyon çağrılarını engelliyorsa, prosedürel stile başvurun.

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Aşağıdaki ayrıntılı kuralların çoğu, bu tavsiyenin daha spesifik varyasyonlarıdır.

#### RECEIVING anahtar kelimesini kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#receiving-anahtar-kelimesini-kullanmayın)

```ABAP
DATA(sum) = aggregate_values( values ).
```

gereksiz yere daha uzun olan yerine

```ABAP
" anti-pattern
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### Opsiyonel olan EXPORTING anahtar kelimesini atlayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#opsiyonel-olan-exporting-anahtar-kelimesini-atlayın)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

gereksiz yere daha uzun olan yerine

```ABAP
" anti-pattern
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Tek parametreli çağrılarda parametre ismini yazmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#tek-parametreli-çağrılarda-parametre-ismini-yazmayın)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

gereksiz yere daha uzun olan yerine

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates( list = list ).
```

Ancak öyle durumlar vardır ki, yalnızca metod ismi yeterince açık değildir
ve parametre adını tekrar etmek, anlaşılabilirliği artırabilir:

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### Bir örnek niteliği (attribute) veya metodu çağırırken ME öz referansını kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Çağrılar (Calls)](#çağrılar-calls) > [Bu bölüm](#bir-örnek-niteliği-attribute-veya-metodu-çağırırken-me-öz-referansını-kullanmayın)

Kendine referans olan `me->`, sistem tarafından örtük olarak ayarlandığı için, bir instance attribute veya metoda erişirken kullanmayın.

```ABAP
DATA(sum) = aggregate_values( values ).
```

gereksiz yere daha uzun olan yerine

```ABAP
" anti-pattern
DATA(sum) = aggregate_values( me->values ).
```

```ABAP
" anti-pattern
DATA(sum) = me->aggregate_values( values ).
```

bir yerel değişken veya importing parametre ile bir instance attribute arasında scope çakışması yoksa

```ABAP
me->logger = logger.
```

### Metotlar: Nesne Yönelimli (Object Orientation)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#metodlar-nesne-yönelimli-object-orientation)

#### Statik metodlar yerine örnek (instance) metodları tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Metotlar: Nesne Yönelimli (Object Orientation)] > [Bu bölüm](#statik-metodlar-yerine-örnek-instance-metodları-tercih-edin)

Metotlar varsayılan olarak instance üye olmalıdır.
Instance metodlar sınıfın "nesne" doğasını daha iyi yansıtır.
Ayrıca, birim testlerde daha kolay mock’lanabilirler.

```ABAP
METHODS publish.
```

Metotlar yalnızca istisnai durumlarda statik olmalıdır,
örneğin statik oluşturma metodları gibi.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### Herkese açık (public) örnek (instance) metodlar bir arayüzün (interface) parçası olmalıdır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Metotlar: Nesne Yönelimli (Object Orientation)] > [Bu bölüm](#herkese-açık-public-örnek-instance-metodlar-bir-arayüzün-interface-parçası-olmalıdır)

Public instance metodları her zaman bir interface’in parçası olmalıdır.
Bu, bağımlılıkları gevşetir ve birim testlerinde onları mock’lamayı kolaylaştırır.

```ABAP
METHOD /clean/blog_post~publish.
```

Clean nesne yöneliminde, bir metodu interface olmadan public yapmak çok anlamlı değildir
bunun birkaç istisnası vardır; örneğin, alternatif implementasyonu olmayacak ve testlerde hiç mock’lanmayacak enumeration sınıfları gibi.

> [Interfaces vs. abstract classes](sub-sections/InterfacesVsAbstractClasses.md)
> neden bunun kalıtılmış metodları override eden sınıflar için de geçerli olduğunu açıklar.

### Parametre Sayısı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#parametre-sayısı)

#### IMPORTING parametre sayısını az tutmaya çalışın, ideal olarak üçten az olsun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Sayısı](#parametre-sayısı) > [Bu bölüm](#importing-parametre-sayısını-az-tutmaya-çalışın-ideal-olarak-üçten-az-olsun)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

çok daha açık olurdu, yerine

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

Çok fazla input parametresi, metodun karmaşıklığını patlatır;
çünkü bu, üssel sayıda kombinasyonu yönetmesini gerektirir.
Çok fazla parametre, metodun birden fazla iş yapıyor olabileceğinin göstergesidir.

Parametre sayısını, onları anlamlı gruplara (structure’lar ve object’ler ile) birleştirerek azaltabilirsiniz.

#### Opsiyonel parametreler eklemek yerine metodları bölün
> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Sayısı](#parametre-sayısı) > [Bu bölüm](#opsiyonel-parametreler-eklemek-yerine-metodları-bölün)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

istenen anlamı sağlamak için, çünkü ABAP [overloading](https://en.wikipedia.org/wiki/Function_overloading) desteklemez.

```ABAP
" anti-pattern
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

Opsiyonel parametreler çağıranları kafa karıştırır:

- Hangileri gerçekten zorunludur?
- Hangi kombinasyonlar geçerlidir?
- Hangileri birbirini dışlar?

Kullanım durumuna özel parametrelerle birden fazla metod tanımlamak,
geçerli ve beklenen parametre kombinasyonları konusunda net rehberlik sağlayarak bu karışıklığı önler.

#### PREFERRED PARAMETER ifadesini çok nadiren kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Sayısı](#parametre-sayısı) > [Bu bölüm](#preferred-parameter-ifadesini-çok-nadiren-kullanın)

`PREFERRED PARAMETER` eklemesi, hangi parametrenin gerçekten verildiğini görmekte zorluk çıkarır,
bu da kodun anlaşılmasını güçleştirir.
Parametre sayısını, özellikle opsiyonel olanları, en aza indirgemek
`PREFERRED PARAMETER` ihtiyacını otomatik olarak azaltır.

#### RETURN, EXPORT veya CHANGE işlemlerinden yalnızca bir parametre üzerinde işlem yapın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Sayısı](#parametre-sayısı) > [Bu bölüm](#return-export-veya-change-işlemlerinden-yalnızca-bir-parametre-üzerinde-işlem-yapın)

İyi bir metod _tek bir işi_ yapar ve bu, metodun tam olarak bir şey döndürmesiyle de yansıtılmalıdır.
Metodunuzun çıktı parametreleri _mantıksal bir bütün_ oluşturmuyorsa,
metodunuz birden fazla işi yapıyor demektir ve bölünmelidir.

Çıktının birden fazla parçadan oluşan mantıksal bir bütün olduğu durumlar vardır.
Bunlar, bir yapı (structure) veya nesne (object) döndürmekle en kolay şekilde temsil edilir:

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

yerine

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

Özellikle birden çok EXPORTING parametreye kıyasla, bu sayede fonksiyonel çağrı stilini kullanmak mümkün olur,
`IS SUPPLIED` kontrolüyle uğraşmanıza gerek kalmaz ve kullanıcıların hayati önemdeki `ERROR_OCCURRED` bilgisini almadan unutmalarını önler.

Birden fazla opsiyonel çıktı parametresi yerine, metodu anlamlı çağrı kalıplarına göre bölmeyi düşünün:

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

### Parametre Tipleri

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#parametre-tipleri)

#### EXPORTING yerine RETURNING kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Tipleri](#parametre-tipleri) > [Bu bölüm](#exporting-yerine-returning-kullanmayı-tercih-edin)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

gereksiz yere daha uzun olan yerine

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

`RETURNING` sadece çağrıyı kısaltmakla kalmaz,
aynı zamanda method chaining’e olanak tanır ve
[same-input-and-output errors](#take-care-if-input-and-output-could-be-the-same) hatalarının önüne geçer.


#### RETURNING ile büyük tablolar döndürmek genelde sorun değildir

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Tipleri](#parametre-tipleri) > [Bu bölüm](#retruning-ile-büyük-tablolar-döndürmek-genelde-sorun-değildir)

ABAP dil dokümantasyonu ve performans kılavuzları aksi yönde belirtse de,
büyük veya derin iç içe geçmiş tabloları VALUE parametresi olarak geçirmek
_gerçekten_ performans sorunlarına yol açan durumlarla nadiren karşılaşırız.
Bu nedenle genellikle şunu kullanmanızı öneririz:

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = large_table.
ENDMETHOD.

DATA(my_table) = get_large_table( ).
```

Sadece bireysel durumunuz için gerçek bir kanıt (= kötü bir performans ölçümü) varsa,
daha zahmetli prosedürel stile başvurmalısınız.

```ABAP
" anti-pattern
METHODS get_large_table
  EXPORTING
    result TYPE /dirty/some_table_type.

METHOD get_large_table.
  result = large_table.
ENDMETHOD.

get_large_table( IMPORTING result = DATA(my_table) ).
```

> Bu bölüm, büyük tabloların performans kaybını önlemek için referansla EXPORT edilmesi gerektiğini öneren
> ABAP Programlama Kılavuzları ve Code Inspector kontrolleri ile çelişmektedir.
> Biz ise herhangi bir performans veya bellek kaybını tutarlı şekilde çoğaltamadık ve
> çekirdek optimizasyonu hakkında geri bildirim aldık; bu optimizasyon genel olarak RETURNING performansını artırır,
> ayrıntılar için [_Sharing Between Dynamic Data Objects_](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenmemory_consumption_3.htm) ABAP Dil Yardımına bakınız.

#### RETURNING, EXPORTING veya CHANGING parametrelerinden yalnızca birini kullanın, kombinasyon yapmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Tipleri](#parametre-tipleri) > [Bu bölüm](#returning-exporting-veya-changing-parametrelerinden-yalnızca-birini-kullanın-kombinasyon-yapmayın)

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

karışıklık yaratan karışımlar yerine

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

Farklı türlerde çıktı parametreleri, metodun birden fazla işi yaptığını gösterir.
Bu, okuyucuyu karıştırır ve metodun çağrılmasını gereksiz yere karmaşıklaştırır.

Bu kurala kabul edilebilir bir istisna, çıktılarını oluştururken girdilerini tüketen builder’lar olabilir:

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

Ancak, bu tür durumlar bile girdiyi nesneleştirerek daha anlaşılır hale getirilebilir:

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### CHANGING parametrelerini sadece uygun olduğu yerlerde ve dikkatli kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Tipleri](#parametre-tipleri) > [Bu bölüm](#changing-parametrelerini-sadece-uygun-olduğu-yerlerde-ve-dikkatli-kullanın)

`CHANGING`, yalnızca hali hazırda dolu olan ve bazı yerlerde güncellenen yerel değişkenler için kullanılmalıdır:

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

Çağıranları, `CHANGING` parametrenizi sağlamak için gereksiz yerel değişkenler tanımlamaya zorlamayın.
`CHANGING` parametrelerini, önceden boş olan bir değişkeni ilk kez doldurmak için kullanmayın.

#### Boolean giriş parametresi kullanmak yerine metodu bölün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Tipleri](#parametre-tipleri) > [Bu bölüm](#boolean-giriş-parametresi-kullanmak-yerine-metodu-bölün)

Boolean input parametreleri genellikle bir metodun bir tane değil, _iki_ işi yaptığının göstergesidir.

```ABAP
" anti-pattern
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

Ayrıca, tek ve dolayısıyla isimsiz olan Boolean parametrelerle yapılan metod çağrıları, parametrenin anlamını gizleme eğilimindedir.

```ABAP
" anti-pattern
update( abap_true ).  " what does 'true' mean? synchronous? simulate? commit?
```

Metodun bölünmesi, metodların kodunu basitleştirebilir
ve farklı niyetleri daha iyi ifade edebilir.

```ABAP
update_without_saving( ).
update_and_save( ).
```

Yaygın algı, Boolean değişkenler için setter metodlarının uygun olduğu yönündedir:

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

> Daha fazla bilgi için
> [1](https://web.archive.org/web/20190907112758/http://www.beyondcode.org/articles/booleanVariables.html)
> [2](https://web.archive.org/web/20220314024954/https://silkandspinach.net/2004/07/15/avoid-boolean-parameters/)
> [3](https://web.archive.org/web/20231211152320/https://jlebar.com/2011/12/16/Boolean_parameters_to_API_functions_considered_harmful..html)

### Parametre İsimleri

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#parametre-isimleri)

#### RETURNING parametresine RESULT adını vermeyi düşünün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre İsimleri](#parametre-isimleri) > [Bu bölüm](#returning-parametresine-result-adını-vermeyi-düşünün)

İyi metod isimleri genellikle o kadar iyidir ki, `RETURNING` parametresinin ayrı bir isme ihtiyacı olmaz.
Parametre ismi, metod adını tekrar etmekten veya açık olan bir şeyi yansıtmaktan öteye gitmez.

Üye adını tekrar etmek, bazen `me->` gibi gereksiz nitelendiricilerin eklenmesini gerektiren çakışmalara yol açabilir.

```ABAP
" anti-pattern
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

Bu durumlarda, parametreyi basitçe `RESULT` olarak adlandırın;
veya tercih ederseniz Hungarian notation kullanarak `RV_RESULT` diyebilirsiniz.

`RETURNING` parametresini, neyi temsil ettiğinin açık olmadığı durumlarda isimlendirin;
örneğin, method chaining için `me` döndüren metodlarda ya da
oluşturulan nesneyi değil, yalnızca anahtarını döndüren metodlarda.

### Parametre Başlatma (Initialization)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#parameter-başlatma-initialization)

#### EXPORTING referans parametrelerini temizleyin veya üzerine yazın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Başlatma (Initialization)](#parameter-başlatma-initialization) > [Bu bölüm](#exporting-referans-parametrelerini-temizleyin-veya-üzerine-yazın)

Referans parametreler, önceden doldurulmuş olabilecek mevcut bellek alanlarını referans eder.
Güvenilir veri sağlamak için bunları temizleyin veya üzerine yazın:

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

> Code Inspector ve Checkman, hiç yazılmayan `EXPORTING` değişkenlerini işaret eder.
> Bu statik kontrolleri kullanarak aksi halde oldukça gizli kalabilecek bu hata kaynağından kaçının.

##### Girdi ve çıktının aynı olabileceği durumlarda dikkatli olun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Başlatma (Initialization)](#parameter-başlatma-initialization) > [EXPORTING referans parametrelerini temizleyin veya üzerine yazın](#exporting-referans-parametrelerini-temizleyin-veya-üzerine-yazın) > [Bu bölüm](#girdi-ve-çıktının-aynı-olabileceği-durumlarda-dikkatli-olun)

Genel olarak, parametreyi metodun içinde (tip ve veri tanımlarından sonra)
ilk iş olarak temizlemek (clear) iyi bir fikirdir.
Bu, ifadeyi kolayca fark edilebilir kılar ve sonrasında kalan değerin kazara kullanılmasını önler.

Ancak, bazı parametre yapılandırmalarında aynı değişken hem input hem de output için kullanılabilir.
Bu durumda, erken bir `CLEAR`, input değerini kullanılmadan önce siler ve yanlış sonuçlara yol açar.

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

Böyle metodları, `EXPORTING` yerine `RETURNING` kullanarak yeniden tasarlamayı düşünün.
Ayrıca, `EXPORTING` parametresini tek bir sonuç hesaplama ifadesinde doğrudan üzerine yazmayı da değerlendirin.
Eğer bunların hiçbiri uygun değilse, geç bir aşamada `CLEAR` kullanın.

#### VALUE parametrelerini temizlemeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Parametre Başlatma (Initialization)](#parameter-başlatma-initialization) > [Bu bölüm](#value-parametrelerini-temizlemeyin)

`VALUE` ile çalışan parametreler, tanımı gereği yeni ve ayrı bellek alanı olarak aktarılır ve zaten boştur.
Bunları tekrar temizlemeyin:

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

`RETURNING` parametreleri her zaman `VALUE` parametreleridir, bu yüzden onları temizlemeniz (clear) gerekmez:

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " no need to CLEAR result
ENDMETHOD.
```

### Metot Gövdesi

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#metod-gövdesi)

#### Tek bir işi yapın, iyi yapın, sadece onu yapın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Metot Gövdesi](#metod-gövdesi) > [Bu bölüm](#tek-bir-işi-yapın-iyi-yapın-sadece-onu-yapın)

Bir metod tek bir işi yapmalıdır ve bunu mümkün olan en iyi şekilde gerçekleştirmelidir.

Bir metod, büyük olasılıkla tek bir işi yapıyordur eğer:

- [az sayıda input parametresi](#importing-parametre-sayısını-az-tutmaya-çalışın-ideal-olarak-üçten-az-olsun) varsa,
- [Boolean parametre içermiyorsa](#boolean-giriş-parametresi-kullanmak-yerine-metodu-bölün),
- [tam olarak bir output parametresi](#return-export-veya-change-işlemlerinden-yalnızca-bir-parametre-üzerinde-işlem-yapın) varsa,
- [küçük](#metodları-kısa-tutun) ise,
- [tek bir soyutlama seviyesine iniyorsa](#sadece-bir-soyutlama-seviyesine-inin),
- yalnızca [tek bir tür istisna](#tek-bir-türde-istisna-fırlatın) fırlatıyorsa,
- içinden anlamlı başka metodlar çıkarılamıyorsa,
- ifadeleri anlamlı bölümlere ayrılamıyorsa.


#### Ya mutlu akışı (happy path) ya da hata yönetimini ele alın, ikisini birden değil

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Metot Gövdesi](#metod-gövdesi) > [Bu bölüm](#ya-mutlu-akışı-happy-path-ya-da-hata-yönetimini-ele-alın-ikisini-birden-değil)

[_Tek bir işi yapın, iyi yapın, sadece onu yapın_](#tek-bir-işi-yapın-iyi-yapın-sadece-onu-yapın) kuralının bir uzantısı olarak,
bir metod ya kendi için tasarlanan “happy path”i izlemeli,
ya da yapamadığı durumda hata işleme yoluna sapmalı,
ama büyük ihtimalle ikisini birden yapmamalıdır.

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

şuna ayrıştırılabilir

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

veya, doğrulama kısmını vurgulamak için

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

#### Sadece bir soyutlama seviyesine inin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Metot Gövdesi](#metod-gövdesi) > [Bu bölüm](#sadece-bir-soyutlama-seviyesine-inin)

Bir metodun içindeki ifadeler, metodun soyutlama seviyesinden bir seviye aşağıda olmalıdır.
Aynı şekilde, bu ifadelerin tümü aynı soyutlama seviyesinde olmalıdır.

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

karışık ve kafa karıştırıcı düşük seviye (`trim`, `to_upper`, ...) ve yüksek seviye (`publish`, ...) kavramlar yerine

```ABAP
" anti-pattern
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

Doğru soyutlama seviyesini bulmanın güvenilir bir yolu şudur:
Metodun yazarına, koda bakmadan metodun ne yaptığını birkaç kısa kelimeyle açıklamasını isteyin.
Numaralandırdığı maddeler, metodun çağırması gereken alt metodları veya çalıştırması gereken ifadeleri gösterir.

#### Metotları kısa tutun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Metot Gövdesi](#metod-gövdesi) > [Bu bölüm](#metodları-kısa-tutun)

Metotlar 20’den az ifade (statement) içermelidir, ideal olarak 3 ila 5 ifade civarında olmalıdır.

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

Aşağıdaki `DATA` bildirimi tek başına bile, çevresindeki metodun birden fazla iş yaptığı izlenimini vermeye yeterlidir:

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

Elbette, daha büyük bir metodu daha fazla küçültmenin anlamlı olmadığı durumlar da vardır.
Bu tamamen kabul edilebilir yeter ki metod [tek bir işe odaklanmış](#tek-bir-işi-yapın-iyi-yapın-sadece-onu-yapın) kalsın:

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

Yine de, uzun ve detaylı kodun daha uygun bir pattern’i gizleyip gizlemediğini kontrol etmek faydalı olur:

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

> Metotları çok küçük parçalara ayırmak, metod çağrılarının sayısını artırdığı için performansa olumsuz etkide bulunabilir.
> [ _Performansa dikkat edin_](#performansa-dikkat-edin) bölümü, Clean Code prensipleri ile performans arasında nasıl denge kurulacağına dair rehberlik sunar.


### Kontrol Akışı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Bu bölüm](#kontrol-akışı)

#### Hızlı hata alın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Kontrol Akışı](#kontrol-akışı) > [Bu bölüm](#hızlı-hata-alın)

Mümkün olan en erken aşamada doğrula ve başarısız ol:

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

Daha sonraki doğrulamalar fark edilmesi ve anlaşılması daha zordur ve o noktaya gelene kadar kaynaklar boşa harcanmış olabilir.

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

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Kontrol Akışı](#kontrol-akışı) > [Bu bölüm](#check-vs-return)

Girdi beklentileri karşılanmadığında metodtan çıkmak için `CHECK` mi yoksa `RETURN` mı kullanılacağı konusunda bir görüş birliği yoktur.

`CHECK` kesinlikle daha kısa bir sözdizimi sağlar.

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " do whatever needs doing
ENDMETHOD.
```

ifadesinin adı, koşul başarısız olursa ne olacağını açıklamaz,
bu yüzden insanlar büyük olasılıkla uzun olan formu daha iyi anlayacaktır:

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " do whatever needs doing
ENDMETHOD.
```

Doğrulamayı tersine çevirip tek dönüşlü (single-return) kontrol akışını benimseyerek bu sorunu tamamen önleyebilirsiniz.
Ancak bu, gereksiz iç içe (nested) yapılar oluşturduğu için bir anti-pattern olarak kabul edilir.

```ABAP
METHOD read_customizing.
  " anti-pattern
  IF keys IS NOT INITIAL.
    " do whatever needs doing
  ENDIF.
ENDMETHOD.
```

Her durumda, hiçbir şey döndürmemenin gerçekten uygun davranış olup olmadığını değerlendirin.
Metotlar anlamlı bir sonuç sağlamalıdır; bu ya dolu bir return parametresi ya da bir istisna (exception) olmalıdır.
Hiçbir şey döndürmek, birçok durumda `null` döndürmekle benzerlik gösterir ve bu durumdan kaçınılmalıdır.

> ABAP Programlama Kılavuzları’ndaki [_Exiting Procedures_ bölümü](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexit_procedure_guidl.htm)
> bu durumda `CHECK` kullanımını önerir.
> Ancak topluluk tartışmaları, bu ifadenin davranışı açıklamakta çok belirsiz olduğu ve
> birçok kişinin programın akışını anlayamayabileceği yönündedir.


#### CHECK ifadesini başka yerlerde kullanmaktan kaçının

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Metotlar](#metodlar) > [Kontrol Akışı](#kontrol-akışı) > [Bu bölüm](#check-ifadesini-başka-yerlerde-kullanmaktan-kaçının)

`CHECK` ifadesini, metodun başlatma (initialization) bölümü dışında kullanmayın.
Bu ifade, bulunduğu yere göre farklı davranış sergiler ve belirsiz, beklenmedik etkilere yol açabilir.

Örneğin,
[`CHECK` bir `LOOP` içinde kullanıldığında, mevcut iterasyonu sonlandırır ve bir sonraki iterasyona geçer](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapcheck_loop.htm);
ancak geliştiriciler, bunun metodu veya döngüyü tamamen sonlandırmasını yanlışlıkla bekleyebilir.
Bu gibi durumlarda, döngü içinde yalnızca kullanılabilen `CONTINUE` komutuyla birlikte bir `IF` ifadesi tercih edilmelidir.

> Bu öneri, ABAP Programlama Kılavuzları’ndaki [_Exiting Procedures_ bölümüne](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexit_procedure_guidl.htm) dayanmaktadır.
> Bunun, [`CHECK`’in döngülerdeki kullanımıyla ilgili keyword referansı](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapcheck_loop.htm) ile çeliştiğine dikkat edin.


## Hata Yönetimi

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#hata-yönetimi)

### Mesajlar

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Bu bölüm](#mesajlar)

#### Mesajları bulunması kolay olsun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Messages](#messages) > [Bu bölüm](#mesajların-bulunması-kolay-olsun)

Mesajları transaction SE91 üzerinden where-used search ile kolayca bulunabilir hale getirmek için aşağıdaki kalıbı kullanın:

```ABAP
MESSAGE e001(ad) INTO DATA(message).
```

Eğer `message` değişkenine ihtiyaç yoksa, aşağıdaki gibi `##NEEDED` pragmasını ekleyin:

```ABAP
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.
```

Aşağıdakinden kaçının:

```ABAP
" anti-pattern
IF 1 = 2. MESSAGE e001(ad). ENDIF.
```

Bu bir anti-pattern’dir çünkü:

- Ulaşılamayan kod içerir.
- Asla doğru olamayacak bir koşulu eşitlik için test eder.


### Dönüş Kodları

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Bu bölüm](#dönüş-kodları)

#### Dönüş kodları yerine istisnaları (exception) tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Return Codes](#return-codes) > [Bu bölüm](#dönüş-kodları-yerine-istisnaları-exception-tercih-edin)

```ABAP
METHOD try_this_and_that.
  RAISE EXCEPTION NEW cx_failed( ).
ENDMETHOD.
```

şunun yerine:

```ABAP
" anti-pattern
METHOD try_this_and_that.
  error_occurred = abap_true.
ENDMETHOD.
```

Exception kullanmanın return code’lara göre birçok avantajı vardır:

- Exception’lar metod imzalarını temiz tutar:
  Metodun sonucunu `RETURNING` parametresiyle döndürürken, aynı zamanda exception fırlatabilirsiniz.
  Return code’lar, hata yönetimi için ek parametrelerle imzayı gereksiz yere kalabalıklaştırır.

- Çağıranın exception’a hemen tepki vermesi gerekmez.
  Kendi kodunun happy path’ini doğrudan yazabilir.
  Exception yönetimi için `CATCH` ifadesi metodunun en sonunda veya tamamen dışında olabilir.

- Exception’lar hata detaylarını kendi attribute’leri ve metodlarıyla verebilir.
  Return code kullanıldığında, log gibi farklı bir çözümü kendiniz geliştirmeniz gerekir.

- Çevre (derleyici), çağıranı syntax hatalarıyla exception’ları ele alması gerektiği konusunda uyarır.
  Return code’lar ise kimse fark etmeden yanlışlıkla görmezden gelinebilir.


#### Hataların sessizce geçmesine izin vermeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Return Codes](#return-codes) > [Bu bölüm](#hataların-sessizce-geçmesine-izin-vermeyin)

Eğer return code kullanmanız gerekiyorsa, örneğin kontrolünüz dışındaki Function’ları veya eski kodları çağırıyorsanız,
hataların gözden kaçmasına izin vermediğinizden emin olun.

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

### İstisnalar (Exceptions)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Bu bölüm](#istisnalar-exceptions)

#### İstisnalar hatalar içindir, normal durumlar için değil

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [İstisnalar (Exceptions)](#istisnalar-exceptions)> [Bu bölüm](#istisnalar-hatalar-içindir-normal-durumlar-için-değil)

```ABAP
" anti-pattern
METHODS entry_exists_in_db
  IMPORTING
    key TYPE char10
  RAISING
    cx_not_found_exception.
```

Bir durum düzenli ve geçerli bir durumsa, bu durum regular sonuç parametreleriyle ele alınmalıdır.

```ABAP
METHODS entry_exists_in_db
  IMPORTING
    key           TYPE char10
  RETURNING
    VALUE(result) TYPE abap_bool.
```

Exception’lar, beklenmeyen ve hata durumunu yansıtan durumlar için saklanmalıdır.

```ABAP
METHODS assert_user_input_is_valid
  IMPORTING
    user_input TYPE string
  RAISING
    cx_bad_user_input.
```

Exception’ların yanlış kullanımı, okuyucuyu bir hata olduğu izlenimine sürüklerken aslında her şey yolundadır.
Ayrıca exception’lar, oluşturulmaları ve çoğunlukla çok fazla bağlam bilgisi toplamaları gerektiği için regular koddan çok daha yavaştır.

#### Sınıf tabanlı (class-based) istisnalar kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [İstisnalar (Exceptions)](#istisnalar-exceptions)> [Bu bölüm](#sınıf-tabanlı-class-based-istisnalar-kullanın)

```ABAP
TRY.
    get_component_types( ).
  CATCH cx_has_deep_components_error.
ENDTRY.
```

Eski, class tabanlı olmayan exception’lar return code’lar ile aynı özelliklere sahiptir ve artık kullanılmamalıdır.

```ABAP
" anti-pattern
get_component_types(
  EXCEPTIONS
    has_deep_components = 1
    OTHERS              = 2 ).
```

### Fırlatma (Throwing)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Bu bölüm](#fırlatma-throwing)

#### Kendi üst sınıflarınızı (super class) kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#kendi-üst-sınıflarınızı-super-class-kullanın)

```ABAP
CLASS cx_fra_static_check DEFINITION ABSTRACT INHERITING FROM cx_static_check.
CLASS cx_fra_no_check DEFINITION ABSTRACT INHERITING FROM cx_no_check.
```

Kendi uygulamanız için, foundation class’lardan doğrudan alt sınıf türetmek yerine,
her bir exception türü için soyut (abstract) üst sınıflar oluşturmayı düşünün.
Bu sayede _kendi_ exception’larınızın tümünü `CATCH` edebilirsiniz.
Tüm exception’lara ortak işlevler eklemenizi sağlar, örneğin özel metin işlemleri gibi.
`ABSTRACT`, insanların bu açıklayıcı olmayan hataları doğrudan kullanmasını engeller.

#### Tek bir türde istisna fırlatın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#tek-bir-türde-istisna-fırlatın)

```ABAP
METHODS generate
  RAISING
    cx_generation_error.
```

Çoğu durumda, birden fazla exception tipi fırlatmanın faydası yoktur.
Çağıran genellikle hata durumlarını ayırt etmekle ilgilenmez veya bunu yapamaz.
Bu nedenle genellikle tüm hata durumlarını aynı şekilde işler -
eğer durum buysa, neden baştan ayırmak gerekir?

```ABAP
" anti-pattern
METHODS generate
  RAISING
    cx_abap_generation
    cx_hdbr_access_error
    cx_model_read_error.
```

Farklı hata durumlarını tanımak için daha iyi bir çözüm, tek bir exception tipi kullanmak,
ancak çağıranın bireysel hata durumlarına tepki vermesine olanak tanıyan
zorunlu olmayan alt sınıflar (sub-classes) eklemektir;
bu konu [Çağıranların (callers) hata durumlarını ayırt edebilmesi için alt sınıflar (sub classes) kullanın](#çağıranların-callers-hata-durumlarını-ayırt-edebilmesi-için-alt-sınıflar-sub-classes-kullanın) bölümünde anlatılmıştır.

#### Çağıranların (callers) hata durumlarını ayırt edebilmesi için alt sınıflar (sub classes) kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#çağıranların-callers-hata-durumlarını-ayırt-edebilmesi-için-alt-sınıflar-sub-classes-kullanın)

```ABAP
CLASS cx_bad_generation_variable DEFINITION INHERITING FROM cx_generation_error.
CLASS cx_bad_code_composer_template DEFINITION INHERITING FROM cx_generation_error.

METHODS generate RAISING cx_generation_error.

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

Eğer birçok farklı hata durumu varsa, bunun yerine hata kodları kullanın:

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

#### Yönetilebilir istisnalar için CX_STATIC_CHECK fırlatın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#yönetilebilir-istisnalar-için-cx_static_check-fırlatın)

Eğer bir exception beklenebilir ve alıcı tarafından makul şekilde işlenebiliyorsa,
`CX_STATIC_CHECK`’ten türeyen checked exception fırlatın:
başarısız kullanıcı girişi doğrulaması, yedekleri olan eksik kaynaklar vb.

```ABAP
CLASS cx_file_not_found DEFINITION INHERITING FROM cx_static_check.

METHODS read_file
  IMPORTING
    file_name_enterd_by_user TYPE string
  RAISING
    cx_file_not_found.
```

Bu exception tipi, metod imzalarında _zorunlu olarak_ belirtilmeli ve
syntax hatası olmaması için _mutlaka_ yakalanmalı veya iletilmelidir.
Bu sayede tüketici için açık olur ve beklenmedik exception’lar karşısında şaşırmaz,
hata durumuna uygun tepkiyi alır.

> Bu, [ABAP Programlama Kılavuzları](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenexception_category_guidl.htm) ile uyumludur
> ancak [Robert C. Martin’in _Clean Code_](sub-sections/Exceptions.md) kitabında önerilen
> unchecked exception’ları tercih etme önerisiyle çelişir;
> nedenleri [Exceptions](sub-sections/Exceptions.md) bölümünde açıklanmıştır.

#### Geri dönülemeyen durumlar için CX_NO_CHECK fırlatın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#geri-dönülemeyen-durumlar-için-cx_no_check-fırlatın)

Eğer bir exception o kadar ciddi ise ki alıcının bundan kurtulması mümkün değilse, `CX_NO_CHECK` kullanın:
zorunlu bir kaynağın okunamaması, istenen bağımlılığın çözümlenememesi gibi durumlar.

```ABAP
CLASS cx_out_of_memory DEFINITION INHERITING FROM cx_no_check.

METHODS create_guid
  RETURNING
    VALUE(result) TYPE /bobf/conf_key.
```

`CX_NO_CHECK` metod imzalarında _bildirilemez_,
bu nedenle oluşumu tüketici için kötü bir sürpriz olur.
Kurtarılamaz durumlarda bu kabul edilebilir, çünkü tüketici bu durumda işe yarar bir şey yapamaz.

Ancak, bazı durumlarda tüketici bu tür hataları gerçekten tanıyıp tepki vermek isteyebilir.
Örneğin, bir dependency manager istenen bir interface için implementasyon sağlayamazsa `CX_NO_CHECK` fırlatabilir,
çünkü normal uygulama kodu devam edemez.
Ama test raporu, tüm nesnelerin oluşturulup oluşturulmadığını kontrol etmek için bu hatayı sadece listeye kırmızı giriş olarak raporlayabilir -
bu servis, exception’ı yakalayıp görmezden gelmelidir, dump olmaya zorlanmamalıdır.

#### Önlenebilir istisnalar için CX_DYNAMIC_CHECK kullanmayı değerlendirin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#önlenebilir-istisnalar-için-cx_dynamic-check-kullanmayı-değerlendirin)

`CX_DYNAMIC_CHECK` kullanımı nadirdir ve genel olarak diğer exception türlerine başvurulması önerilir.
Ancak, çağıranın exception’ın oluşup oluşmayacağı üzerinde tam ve bilinçli kontrolü varsa,
bu tür exception’ı `CX_STATIC_CHECK` yerine değerlendirebilirsiniz.

```ABAP
DATA value TYPE decfloat.
value = '7.13'.
cl_abap_math=>get_db_length_decs(
  EXPORTING
    in     = value
  IMPORTING
    length = DATA(length) ).
```

Örneğin, `cl_abap_math` sınıfının `get_db_length_decs` metodu,
bir ondalık kayan nokta sayısının basamak ve ondalık hane sayısını verir.
Girdi parametresi ondalık kayan nokta sayı değilse, bu metod
dinamik exception `cx_parameter_invalid_type` fırlatır.
Genellikle bu metod, tamamen ve statik olarak tiplenmiş bir değişken için çağrılır,
yani geliştirici bu exception’ın oluşup oluşmayacağını bilir.
Bu durumda dinamik exception, çağıranın gereksiz `CATCH` bloğunu atlamasını sağlar.

#### Tamamen geri dönülemez durumlar için programı dump'a düşürün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#tamamen-geri-dönüleme-durumlar-için-programı-dumpa-düşürün)

Eğer bir durum o kadar ciddi ise ki, alıcının bundan kurtulması pek mümkün değilse
veya bu durum açıkça bir programlama hatasını gösteriyorsa, exception fırlatmak yerine dump oluşturun:
bellek tahsisi başarısızlığı, doldurulması gereken tabloda başarısız indeks okuma gibi durumlar.

```ABAP
RAISE SHORTDUMP TYPE cx_sy_create_object_error.  " >= NW 7.53
MESSAGE x666(general).                           " < NW 7.53
```

Bu davranış, tüketicinin sonrasında herhangi yararlı bir işlem yapmasını engeller.
Bu yüzden yalnızca kesin olduğunuz durumlarda kullanın.

#### RAISE EXCEPTION TYPE yerine RAISE EXCEPTION NEW kullanmayı tercih edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Fırlatma (Throwing)](#fırlatma-throwing) > [Bu bölüm](#raise-exception-type-yerine-raise-exception-new-kullanmayı-tercih-edin)

Not: NW 7.52 ve sonrasında kullanılabilir.

```ABAP
RAISE EXCEPTION NEW cx_generation_error( previous = exception ).
```

genellikle gereksiz uzun olan şunun yerine daha kısadır:

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  EXPORTING
    previous = exception.
```

Ancak, `MESSAGE` ekini yoğun kullanıyorsanız, `TYPE` varyantını tercih edebilirsiniz:

```ABAP
RAISE EXCEPTION TYPE cx_generation_error
  MESSAGE e136(messages)
  EXPORTING
    previous = exception.
```

### Yakalama (Catching)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Bu bölüm](#yakalama-catching)

#### Yabancı (dış) istisnaları sarmalayın, kodunuza sızmalarına izin vermeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Hata Yönetimi](#hata-yönetimi) > [Yakalama (Catching)](#yakalama-catching) > [Bu bölüm](#yabancı-dış-istisnaları-sarmalayın-kodunuza-sızmalarına-izin-vermeyin)

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

[Law of Demeter](https://en.wikipedia.org/wiki/Law_of_Demeter) bağımlılıkları azaltmayı önerir.
Başka bileşenlerden gelen exception’ları doğrudan iletmek bu ilkeyi ihlal eder.
Yabancı koda bağımlılığınızı azaltmak için bu exception’ları yakalayıp,
kendi exception tipinizle sarın.

```ABAP
" anti-pattern
METHODS generate RAISING cx_sy_gateway_failure.

METHOD generate.
  generator->generate( ).
ENDMETHOD.
```

## Yorumlar

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#yorumlar)

### Kodla kendinizi ifade edin, yorumla değil

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#kodla-kendinizi-ifade-edin-yorumla-değil)

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

yerine

```ABAP
" anti-pattern
" örn. artık olmayan yıllarda 29.02. gibi tarihleri düzeltmek veya tarih hesaplaması sonucu
" 31.06. gibi bir değer çıktığında bunu 30.06. olarak düzeltmek
METHOD fix_day_overflow.
  DO 3 TIMES.
    " 31 - 28 = 3 => bu düzeltme en fazla 3 kez yapılır
    lv_dummy = cv_date.
    " lv_dummy, tarih değeri geçersizse 0 olur - ABAP’a özgü uygulama
    IF ( lv_dummy EQ 0 ).
      cv_date+6(2) = cv_date+6(2) - 1. " verilen tarihten 1 gün çıkar
    ELSE.
      " tarih geçerli => düzeltme gerekmez
      EXIT.
    ENDIF.
  ENDDO.
ENDMETHOD.
```

Clean Code kodunuza yorum yapmanızı yasaklamaz aksine _daha iyi_ yöntemler kullanmanızı önerir,
yorum kullanımı ancak bu yöntemler başarısız olursa tercih edilmelidir.

> Bu örnek performans açısından eleştirilmiştir,
> metodları bu kadar küçük parçalara ayırmanın performansı çok düşürdüğü iddia edilmiştir.
> Ölçümler, refaktör edilmiş kodun orijinal kaba koda göre 2.13 kat daha yavaş olduğunu göstermiştir.
> Temiz versiyon `31-02-2018` girdisini düzeltmek için 9.6 mikro saniye, kaba versiyon ise sadece 4.5 mikro saniye almıştır.
> Bu, metod çok sık ve yüksek performans gerektiren uygulamalarda sorun olabilir;
> ancak normal kullanıcı girişi doğrulamasında kabul edilebilir olmalıdır.
> Clean Code ve performans sorunları için [Performansa dikkat edin](#performansa-dikkat-edin) bölümüne göz gezdirebilirsiniz.


### Kötü isimlendirme için yorumları bahane etmeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#kötü-isimlendirme-için-yorumları-bahane-etmeyin)

```ABAP
DATA(input_has_entries) = has_entries( input ).
```

İsminizi açıklamak yerine, anlamını net şekilde ifade edecek şekilde geliştirin.

```ABAP
" anti-pattern
" input tablosunda kayıt olup olmadığını kontrol eder
DATA(result) = check_table( input ).
```

### Kodu bölmek için yorum yerine metod kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#kodu-bölmek-için-yorum-yerine-metod-kullanın)

```ABAP
DATA(statement) = build_statement( ).
DATA(data) = execute_statement( statement ).
```

Bu yaklaşım, kodun amacını, yapısını ve bağımlılıklarını çok daha net hale getirir,
aynı zamanda geçici değişkenlerin bölümler arasında düzgün temizlenmemesinden kaynaklanan hataları önler.

```ABAP
" anti-pattern
" -----------------
" Sorgu oluşturma
" -----------------
DATA statement TYPE string.
statement = |SELECT * FROM d_document_roots|.

" -----------------
" Sorguyu çalıştırma
" -----------------
DATA(result_set) = adbc->execute_sql_query( statement ).
result_set->next_package( IMPORTING data = data ).
```

### Ne yaptığınızı değil, neden yaptığınızı açıklayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#ne-yaptığınızı-değil-neden-yaptığınızı-açıklayın)

```ABAP
" can't fail, existence of >= 1 row asserted above
DATA(first_line) = table[ 1 ].
```

Hiç kimsenin doğal dilde kodu tekrar etmesine gerek yoktur.

```ABAP
" anti-pattern
" anahtar ile veritabanından alert root seçilir
SELECT * FROM d_alert_root WHERE key = key.
```

### Tasarım açıklamaları koda değil, tasarım dökümanlarına yazılır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#tasarım-açıklamaları-koda-değil-tasarım-dökümanlarına-yazılır)

```ABAP
" anti-pattern
" Bu sınıf iki amaç için kullanılıyor. İlk olarak bir işi yapıyor. Sonra başka bir işi yapıyor.
" Bunu, yerel yardımcı sınıflara dağıtılmış çok sayıda kod çalıştırarak yapıyor.
" Neler olup bittiğini anlamak için önce evrenin doğasını düşünelim.
" Detaylar için buna ve şuna bakın.
```

Bunu kimse okumaz — cidden.
İnsanlar kodunuzu kullanmak için ders kitabı okumak zorundaysa,
bu kodunuzda ciddi tasarım sorunları olduğunun işareti olabilir ve başka şekilde çözmeniz gerekir.
Bazı kodlar gerçekten tek satırlık yorumun ötesinde açıklama gerektirir;
bu durumlarda tasarım dokümanına bağlantı vermeyi düşünebilirsiniz.

### " ile yorum yazın, * ile yazmayın 

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#ile-yorum-yazın-ile-yazmayın)

Yorumlar, yorumladıkları ifadelerle aynı girintide olmalıdır

```ABAP
METHOD do_it.
  IF input IS NOT INITIAL.
    " delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

Yıldız işaretiyle yapılan yorumlar garip yerlere kayma eğilimindedir

```ABAP
" anti-pattern
METHOD do_it.
  IF input IS NOT INITIAL.
* delegate pattern
    output = calculate_result( input ).
  ENDIF.
ENDMETHOD.
```

### Yorumları ilgili satırdan önce yazın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#yorumları-ilgili-satırdan-önce-yazın)

```ABAP
" delegate pattern
output = calculate_result( input ).
```

Şunun yerine daha nettir:

```ABAP
" anti-pattern
output = calculate_result( input ).
" delegate pattern
```

Ve şundan daha az müdahalecidir:

```ABAP
output = calculate_result( input ).  " delegate pattern
```

### Kodu yorum satırı yapmak yerine silin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#kodu-yorum-satırı-yapmak-yerine-silin)

```ABAP
" anti-pattern
* output = calculate_result( input ).
```

Böyle bir şeyle karşılaşırsanız, silin.
Kod belli ki gerekli değil çünkü uygulamanız çalışıyor ve tüm testler başarılı.
Silinen kod daha sonra versiyon geçmişinden tekrar elde edilebilir.
Kalıcı olarak bir kod parçasını saklamanız gerekirse, bunu bir dosyaya veya `$TMP` ya da `HOME` objesine kopyalayın.

### Manuel olarak versiyon kontrolü yapmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#manuel-olarak-versiyon-kontrolü-yapmayın)

```ABAP
" anti-pattern
* ticket 800034775 ABC ++ Start
output = calculate_result( input ).
* ticket 800034775 ABC ++ End
```

Atıf yorumları kodu gereksiz yere kirletir ve büyük fayda sağlamaz;
sürüm kontrolü zaten kaynak kod yönetimi tarafından yapılmaktadır.
Bir şeyin neden değiştirildiğini açıklamak için taşıma (transport) emir metinleri çok daha uygundur.

### FIXME, TODO, ve XXX ekleyin ve kullanıcı ID'nizi ekleyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#fixme-todo-ve-xxx-ekleyin-ve-kullanıcı-idnizi-ekleyin)

```ABAP
METHOD do_something.
  " XXX FH delete this method - it does nothing
ENDMETHOD.
```

- `FIXME`, içsel sorunlar için çok küçük veya gelişmekte olan hataları işaret eder.
- `TODO`, yakın(!) gelecekte tamamlanması gereken yerleri gösterir.
- `XXX`, çalışan ancak geliştirilebilecek kodu işaret eder.

Böyle bir yorum eklediğinizde, mesaja kendi takma adınızı, inisiyallerinizi veya kullanıcı adınızı ekleyin;
böylece ekip arkadaşlarınız yorum belirsizse size ulaşabilir ve soru sorabilir.

### Metot imzası ve bitiş yorumları eklemeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#metod-imzası-ve-bitiş-yorumları-eklemeyin)

Metot imzası yorumları kimseye yardımcı olmaz.

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

Yıllar önce, kodu incelerken metod imzasını göremediğinizde veya onlarca sayfalık çıktı ile çalışırken bu yorumlar anlamlı olabilirdi.
Ama günümüzün tüm modern ABAP IDE’leri (SE24, SE80, ADT) metod imzasını kolayca gösterdiği için bu yorumlar sadece kalabalığa dönüşmüştür.

> SE24/SE80’in form tabanlı editöründe _Signature_ butonuna basın.
> ABAP Development Tools’ta metod ismini seçip F2’ye basın veya perspektifinize _ABAP Element Info_ görünümünü ekleyin.

Benzer şekilde, sonundaki yorumlar da gereksizdir.
Bunlar yıllar önce faydalı olabilirdi, çünkü programlar, fonksiyonlar ve iç içe IF’ler yüzlerce satır olabiliyordu.
Ama modern kodlama tarzımız, `ENDIF` veya `ENDMETHOD`’un hangi açılış ifadesine ait olduğunu anlamak için yeterince kısa metodlar üretir:

```ABAP
" anti-pattern
METHOD get_kpi_calc.
  IF has_entries = abap_false.
    result = 42.
  ENDIF.  " IF has_entries = abap_false
ENDMETHOD.   " get_kpi_calc
```

### Mesaj metinlerini yorumla tekrar etmeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#mesaj-metinlerini-yorumla-tekrar-etmeyin)

```ABAP
" anti-pattern
" alert category not filled
MESSAGE e003 INTO dummy.
```

Mesajlar kodunuzdan bağımsız olarak değişir,
ve kimse yorumu güncel tutmayı hatırlamaz,
bu yüzden yorum hızla eskir ve yanıltıcı hale gelir.

Modern IDE’ler mesajın arkasındaki metni kolayca görmenizi sağlar,
örneğin ABAP Development Tools’ta mesaj ID’sini seçip F2’ye basabilirsiniz.

Daha açık olması için, mesajı kendi metoduna çıkarmayı düşünebilirsiniz.

```ABAP
METHOD create_alert_not_found_message.
  MESSAGE e003 INTO dummy.
ENDMETHOD.
```

### ABAP Doc sadece herkese açık API'lerde kullanılmalı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#abap-doc-sadece-herkese-açık-apilerde-kullanılmalı)

Public API’leri belgelemek için ABAP Doc kullanın,
yani başka ekipler veya uygulamalar tarafından kullanılacak geliştiricilere yönelik API’leri.
İç kullanım için ABAP Doc yazmayın.

ABAP Doc, tüm yorumlar gibi aynı zayıflıklara sahiptir;
çabuk eskiyebilir ve yanıltıcı olabilir.
Bu yüzden, sadece anlamlı olduğu durumlarda kullanmalı,
her şeye ABAP Doc yazmayı zorunlu kılmamalısınız.

> Daha fazla bilgi için _Chapter 4: Good Comments: Javadocs in Public APIs_ and _Chapter 4: Bad Comments:
> Javadocs in Nonpublic Code_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Pseudo yorumlar yerine pragma kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Yorumlar](#yorumlar) > [Bu bölüm](#pseudo-yorumlar-yerine-pragma-kullanın)

ATC tarafından tespit edilen alakasız uyarı ve hataları bastırmak için pseudo yorumlar yerine
pragma kullanmayı tercih edin. Pseudo yorumlar büyük ölçüde modası geçmiş olup pragma ile değiştirilmiştir.

```ABAP
" pattern
MESSAGE e001(ad) INTO DATA(message) ##NEEDED.

" anti-pattern
MESSAGE e001(ad) INTO DATA(message). "#EC NEEDED
```

Eski pseudo yorumlar ile onları değiştiren pragma’lar arasındaki eşleştirmeyi bulmak için `ABAP_SLIN_PRAGMAS` programını veya `SLIN_DESC` tablosunu kullanın.


## Biçimlendirme

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#biçimlendirme)

Aşağıdaki öneriler [yazarken değil okurken kolaylık sağlanması amaçlıdır](#yazarken-değil-okurken-kolaylık-sağlayın).
ABAP Formatter bunları kapsamadığı için, isim uzunlukları vb. değiştiğinde bazı ifadelerin yeniden elle formatlanmasını gerektirebilir;
bunu önlemek istiyorsanız,
[aynı nesneye yapılan atamaları hizalayın farklılara değil](#aynı-nesneye-yapılan-atamaları-hizalayın-farklılara-değil) gibi kuralları bırakmayı düşünebilirsiniz.

### Tutarlı olun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#tutarlı-olun)

Bir projenin tüm kodunu aynı biçimde formatlayın.
Tüm ekip üyelerinin aynı formatlama stilini kullanmasını sağlayın.

Başka birinin kodunu düzenliyorsanız,
kendi kişisel stilinizde ısrar etmek yerine o projenin formatlama stiline uyun.

Formatlama kurallarınızı zamanla değiştirirseniz,
kodunuzu zaman içinde güncellemek için [refactoring en iyi uygulamalarını](#how-to-refactor-legacy-code) kullanın.

### Yazarken değil, okurken kolaylık sağlayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#yazarken-değil-okurken-kolaylık-sağlayın)

Geliştiriciler zamanlarının çoğunu kod _okumaya_ harcar.
Aslında kod _yazmak_ günün çok daha küçük bir kısmını alır.

Bu nedenle, kod formatlamanızı yazmak için değil,
okumak ve hata ayıklamak için optimize etmelisiniz.

Örneğin, aşağıdaki yazımı tercih etmelisiniz:

```ABAP
DATA:
  a TYPE b,
  c TYPE d,
  e TYPE f.
```

aşağıdaki gibi hack’lere değil:

```ABAP
" anti-pattern
DATA:
  a TYPE b
  ,c TYPE d
  ,e TYPE f.
```

### Aktifleştirmeden önce ABAP Biçimlendirici'yi kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#aktifleştirmeden-önce-abap-biçimlendiriciyi-kullanın)

Bir nesneyi aktifleştirmeden önce ABAP Formatter’ı — SE80, SE24 ve ADT’de Shift+F1 — uygulayın.
Not: ABAP Formatter, SAP GUI’de Pretty Printer olarak bilinir.

Eğer büyük, formatlanmamış eski bir kod tabanını değiştiriyorsanız,
devasa değişiklik listeleri ve taşıma bağımlılıklarını önlemek için
ABAP Formatter’ı sadece seçili satırlara uygulamak isteyebilirsiniz.
Tam geliştirme nesnesini ayrı bir Transport Request veya Notta formatlamayı düşünün.

> Daha fazla bilgi için _Chapter 5: Formatting: Team Rules_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Ekipteki ortak ABAP Biçimlendirici ayarlarını kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#ekipteki-ortak-abap-biçimlendirici-ayarlarını-kullanın)

Her zaman takımınızın ABAP Formatter ayarlarını kullanın.
Bunları şu şekilde belirleyin:

* Eclipse: Project Explorer’da projeye sağ tıklayın > _Properties_ > _ABAP Development_ > _Editors_ > _Source Code Editors_ > _ABAP Formatter_
* Eclipse (alternatif yol): _Menu_ > _Window_ > _Preferences_ > _ABAP Development_ > _Editors_ > _Source Code Editors_ > Sağ taraftaki _ABAP Formatter_ bağlantısına tıklayın > Açılan pencereden projeyi seçin
* SAP GUI: _Menu_ > _Utilities_ > _Settings ..._ > _ABAP Editor_ > _Pretty Printer_.

Takımınızın anlaşmasına göre _Indent_ ve _Convert Uppercase/Lowercase_ > _Uppercase Keyword_ ayarlarını yapın.

> [Upper vs. Lower Case](sub-sections/UpperVsLowerCase.md) açıklıyor
> neden anahtar kelimelerin büyük/küçük harf kullanımı için net bir yönlendirme yapmadığımızı.
>
> Daha fazla bilgi için _Chapter 5: Formatting: Team Rules_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Bir satırda yalnızca bir ifade olsun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#bir-satırda-yalnızca-bir-ifade-olsun)

```ABAP
DATA do_this TYPE i.
do_this = input + 3.
```

Bazı durumlarda bunun okunabilir olduğu yanılgısına düşülebilir:

```ABAP
" anti-pattern
DATA do_this TYPE i. do_this = input + 3.
```

### Satır uzunluğunu makul seviyede tutun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#satır-uzunluğunu-makul-seviyede-tutun)

Satır uzunluğu en fazla 120 karakter olmalıdır.

İnsan gözü metni daha rahat okur, satırlar çok geniş olmadığında - istediğiniz bir UI tasarımcısına veya göz hareketleri araştırmacısına sorabilirsiniz. Kodunuzu hata ayıklarken veya yan yana iki kaynak kodunu karşılaştırırken dar satırlar daha kullanışlı olur.

Eski terminal cihazlarından gelen 80 ya da 72 karakter sınırı biraz fazla kısıtlayıcıdır. 100 karakter sıklıkla önerilir ve kabul edilebilir bir seçimdir, ancak ABAP için 120 karakter biraz daha iyi çalışıyor gibi görünür; muhtemelen dilin genel uzun yapısından dolayı.

> Hatırlatma olarak, ADT’de yazdırma sınırını 120 karakter olarak ayarlayabilirsiniz,
> bu sınır kod görünümünde dikey bir çizgi olarak gösterilir.
> Ayarı _Menu_ > _Window_ > _Preferences_ > _General_ > _Editors_ > _Text Editors_ yolundan yapabilirsiniz.

### Kodunuzu sadeleştirin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#kodunuzu-sadeleştirin)

```ABAP
DATA(result) = calculate( items ).
```

gereksiz boşluklar eklemek yerine

```ABAP
" anti-pattern
DATA(result)        =      calculate(    items =   items )   .
```

### Ayırmak için yalnızca tek satır boşluk bırakın, fazlasını değil

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#ayırmak-için-yalnızca-tek-satır-boşluk-bırakın-fazlasını-değil)

```ABAP
DATA(result) = do_something( ).

DATA(else) = calculate_this( result ).
```

iki ifadenin farklı işler yaptığını vurgulamak için. Ancak şu şekilde gereksiz boşluklar eklemek için bir neden yoktur:

```ABAP
" anti-pattern
DATA(result) = do_something( ).



DATA(else) = calculate_this( result ).
```

Ayrı boş satırlar ekleme isteği, metodunuzun [tek bir işi yapmadığının](#tek-bir-işi-yapın-iyi-yapın-sadece-onu-yapın) bir göstergesi olabilir.

### Boş satırları takıntı haline getirmeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#boş-satırları-takıntı-haline-getirmeyin)

```ABAP
METHOD do_something.
  do_this( ).
  then_that( ).
ENDMETHOD.
```

Kodunuzu gereksiz boş satırlarla parçalamanıza gerek yoktur:

```ABAP
" anti-pattern
METHOD do_something.

  do_this( ).

  then_that( ).

ENDMETHOD.
```

Aynı durum bir ifade içinde de geçerlidir; çünkü boş satırlar kodu hızlı okurken yeni bir ifade gibi algılanabilir:

```ABAP
" anti-pattern
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )

                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

Boş satırlar sadece ifadeler birden çok satıra yayıldığında anlam kazanır:

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


### Aynı nesneye yapılan atamaları hizalayın, farklılara değil

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#aynı-nesneye-yapılan-atamaları-hizalayın-farklılara-değil)

Birbirine ait olduğunu vurgulamak için

```ABAP
structure-type = 'A'.
structure-id   = '4711'.
```

ya da daha iyisi

```ABAP
structure = VALUE #( type = 'A'
                     id   = '4711' ).
```

Ancak birbiriyle ilgisi olmayanları hizalamadan bırak:

```ABAP
customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
hdb_access         = fra_hdbr_access=>s_get_instance( ).
```

> Daha fazla bilgi için _Chapter 5: Formatting: Horizontal Alignment_ of [Robert C. Martin's _Clean Code_] kısmına bakabilirsiniz.

### Parantezleri satır sonunda kapatın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#parantezleri-satır-sonunda-kapatın)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

gereksiz uzun olan yerine

```ABAP
" anti-pattern
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields
).
```

### Tek parametreleri çağrılar aynı satırda kalmalı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#tek-parametreleri-çağrılar-aynı-satırda-kalmalı)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
remove_duplicates( CHANGING list = list ).
```

gereksiz uzun olan yerine

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates(
                           list ).
DATA(unique_list) = remove_duplicates(
                         CHANGING
                           list = list ).
```

### Parametreler çağrıdan hemen sonra gelmeli

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#parametreler-çağrıdan-hemen-sonra-gelmeli)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Satırlar çok uzun olduğunda, parametreleri yeni satıra alabilirsiniz:

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = round_up( input DIV 7 ) * 42 + round_down( 19 * step_size )
                value_2 = VALUE #( ( `Calculation failed with a very weird result` ) ) ).
```

### Satır kırılıyorsa, parametreleri çağırın altına girintileyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#satır-kırılıyorsa-parametreleri-çağırın-altına-girintileyin)

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Parametreleri başka yerde hizalamak, hangi ifadeye ait olduklarını görmek zorlaştırır:

```ABAP
" anti-pattern
DATA(sum) = add_two_numbers(
    value_1 = 5
    value_2 = 6 ).
```

Ancak, isim uzunluğundaki değişikliklerin biçimlendirmeyi bozmaması için bu en iyi kalıptır.

### Çok sayıda parametre varsa satır kırın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#çok-sayıda-parametre-varsa-satır-kırın)

```ABAP
DATA(sum) = add_two_numbers( value_1 = 5
                             value_2 = 6 ).
```

Evet, bu biraz yer israfı.
Ancak, aksi halde bir parametrenin nerede bittiği ve diğerinin nerede başladığını görmek zor olur:

```ABAP
" anti-pattern
DATA(sum) = add_two_numbers( value_1 = 5 value_2 = 6 ).
```

### Parametreleri hizalayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#parametreleri-hizalayın)

```ABAP
modify->update( node           = if_fra_alert_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

Düzensiz hizalamalar, parametrenin nerede bittiğini ve değerinin nerede başladığını görmekte zorlaştırır:

```ABAP
" anti-pattern
modify->update( node = if_fra_alert_c=>node-item
                key = item->key
                data = item
                changed_fields = changed_fields ).
```

> This is on the other side the best pattern if you want to avoid the formatting to be broken by a name length change.

### Satır çok uzunsa çağrıyı yeni satıra kırın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#satır-çok-uzunsa-çağrıyı-yeni-satıra-kırın)

```ABAP
DATA(some_super_long_param_name) =
  if_some_annoying_interface~add_two_numbers_in_a_long_name(
      value_1 = 5
      value_2 = 6 ).
```

### Tab'la girintileyin ve hizalayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#tabla-girintileyin-ve-hizalayın)

Parametre anahtar kelimelerini 2 boşluk, parametreleri ise 4 boşluk girintile:

```ABAP
DATA(sum) = add_two_numbers(
              EXPORTING
                value_1 = 5
                value_2 = 6
              CHANGING
                errors  = errors ).
```

Anahtar kelime yoksa, parametreleri 4 boşluk ile girintile.

```ABAP
DATA(sum) = add_two_numbers(
                value_1 = 5
                value_2 = 6 ).
```

Girintilemek için Tab tuşunu kullan. Gereğinden bir boşluk fazla eklenmesi sorun değil.
(Bu, soldaki `DATA(sum) =` kısmının karakter sayısı tek sayıysa olur.)

### İç içe tanımalamalar, metod çağrıları gibi girintilenmeli

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#iç-içe-tanımalamalar-metod-çağrıları-gibi-girintilenmeli)

`VALUE` veya `NEW` ile yapılan satır içi tanımlamaları, sanki metod çağrısıymış gibi girintile:

```ABAP
DATA(result) = merge_structures( a = VALUE #( field_1 = 'X'
                                              field_2 = 'A' )
                                 b = NEW /clean/structure_type( field_3 = 'C'
                                                                field_4 = 'D' ) ).
```

### Tip tanımlarında hizalama yapmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#tip-tanımlarında-hizalama-yapmayın)

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

Bir değişken ve tipi birbirine aittir ve görsel olarak yakın tutulmalıdır.
`TYPE` ifadelerini hizalamak dikkati dağıtır ve değişkenlerin bir dikey grup, tiplerin başka bir dikey grup olduğu izlenimini verir.
Hizalama ayrıca gereksiz düzenleme yükü yaratır; en uzun değişken adı uzunluğu değiştiğinde tüm girintileri yeniden ayarlamanız gerekir.

```ABAP
" anti-pattern
DATA name   TYPE seoclsname.
DATA reader TYPE REF TO /clean/reader.
```

### Atamaları zincirleme yapmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Biçimlendirme](#biçimlendirme) > [Bu bölüm](#atamaları-zincirleme-yapmayın)

```ABAP
" anti-pattern
var1 = var2 = var3.
```

Zincirleme atamalar genellikle okuyucuyu şaşırtır. Ayrıca, inline deklarasyon çoklu atamanın herhangi bir pozisyonunda çalışmaz.

```ABAP
var2 = var3.
var1 = var3.
```

Ek olarak, anti-pattern `=` işaretinin ABAP'ta hem karşılaştırma hem atama için kullanılması nedeniyle belirsiz görünür. Bu durum, diğer bazı programlama dillerinde karşılaştırmanın nasıl yapıldığına benzer görünür; örneğin JavaScript'te `a = ( b == c )`. [Boolean değişken atamalarında XSDBOOL kullanın](#boolean-değişken-atamalarında-xsdbool-kullanın)

## Test Etme

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Bu bölüm](#test-etme)

### İlkeler

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#ilkeler)

#### Test edilebilir kod yazın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [İlkeler](#ilkeler) > [Bu bölüm](#test-edilebilir-kod-yazın)

Tüm kodu otomatik test yapılabilecek şekilde yazın.

Eğer bu, kodunuzu yeniden düzenlemeyi (refactoring) gerektiriyorsa, yapın.
Başka özellikler eklemeye başlamadan önce bunu öncelikli olarak gerçekleştirin.

Çok kötü yapılandırılmış ve test edilemeyen eski koda ekleme yapıyorsanız,
en azından yaptığınız eklemeleri test edebileceğiniz kadar yeniden düzenleyin.

#### Başkalarının sizi mock edebilmesini sağlayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [İlkeler](#ilkeler) > [Bu bölüm](#başkalarının-sizi-mock-edebilmesini-sağlayın)

Başka kişiler tarafından kullanılacak kod yazıyorsanız, onların kendi kodları için birim testler yazabilmesini sağlayın, örneğin arayüzler 
(interfaces) ekleyerek, entegrasyon testlerini kolaylaştıran yardımcı test double'lar sağlayarak veya bağımlılık tersine çevirme
(dependency inversion) uygulayarak konfigürasyonun test konfigürasyonu ile değiştirilebilmesine olanak tanıyın.

#### Okunabilirlik kuralları

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [İlkeler](#ilkeler) > [Bu bölüm](#okunabilirlik-kuralları)

Test kodunuzu, üretim kodunuzdan bile daha okunabilir yapın. Kötü üretim kodunu iyi testlerle düzeltebilirsiniz, ama testleri bile anlayamazsanız kaybedersiniz.

Test kodunuzu o kadar basit ve sade tutun ki, bir yıl sonra bile anlayabilin.

Ortak standartlara ve kalıplara bağlı kalın; böylece iş arkadaşlarınız koda hızlıca adapte olabilir.

#### Kopyalama yapmayın, test raporu yazmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [İlkeler](#ilkeler) > [Bu bölüm](#kopyalama-yapmayın-test-raporu-yazmayın)

Backlog maddesine başlamadan önce bir geliştirme objesinin `$TMP` kopyasını alıp üzerinde oynamaya başlamayın. Başkaları bu objeleri fark etmez ve işinizin durumu bilinmez olur. Ayrıca, çalışma kopyasını oluşturmakla gereksiz zaman kaybedersiniz ve sonrasında kopyayı silmeyi unutup sisteminizde ve bağımlılıklarınızda gereksiz yük oluşturursunuz. (İnanmıyorsanız, hemen geliştirme sisteminizde `$TMP` klasörünü kontrol edin.)

Ayrıca, bir iş üzerinde çalışırken, belirli bir şekilde çağıran test raporu yazıp, işi ilerlettikçe aynı testi elle tekrarlayarak doğrulamaya çalışmayın. Bu “fakir adam testi”dir: test raporunu elle tekrar edip gözle her şeyin yolunda olup olmadığını kontrol etmek.

Bunun yerine bir adım ileri gidip bu raporu otomatik birim testine dönüştürün; kodun hâlâ doğru olup olmadığını otomatik olarak bildiren assert’lerle. Böylece önce testleri sonradan yazma zahmetinden kurtulursunuz, sonra da manuel tekrarlarla harcanan zamanı ve sıkılmayı önlersiniz.

#### Private alanları değil public metodları test edin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [İlkeler](#ilkeler) > [Bu bölüm](#private-alanları-değil-public-metodları-test-edin)

Sınıfların özellikle uyguladıkları arayüzler gibi public kısımları genellikle stabildir ve değişmesi pek beklenmez. Birim testlerinizi sadece public kısımları doğrulayacak şekilde yazın; böylece testleriniz sağlam olur ve sınıfı refactor ederken harcayacağınız emek minimuma iner.

Buna karşın protected ve private iç detaylar hızlıca değişebilir; bu yüzden her refaktör işleminde testleriniz gereksiz yere kırılır.

Private veya protected metodları test etme ihtiyacının acil şekilde ortaya çıkması, tasarımda bazı sorunların erken işareti olabilir. Kendinize şu soruları sorun:

- Kazara, kendi başına bir sınıfa dönüşmesi gereken bir konsepti bu sınıfın içine gömdünüz mü? Kendi özel test dizisine sahip olması gereken bir yapı olabilir mi?

- Domain mantığını yapıştırıcı koddan ayırmayı unuttunuz mu? Örneğin, domain mantığını BOPF’e action, determination veya validation olarak bağlanan ya da SAP Gateway tarafından oluşturulan `*_DPC_EXT` data provider sınıfında doğrudan uygulamak en iyi yaklaşım olmayabilir.

- Arayüzleriniz çok mu karmaşık ve gereksiz ya da kolayca mocklanamayan fazla veri mi istiyor?


#### Kapsam (coverage) takıntısı yapmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [İlkeler](#ilkeler) > [Bu bölüm](#kapsam-coverage-takıntısı-yapmayın)

Kod kapsamı, bazı rastgele KPI’ları karşılamak için değil, test etmeyi unuttuğunuz kodları bulmanız için vardır.

Test kapsamını artırmak için anlamsız veya sahte doğrulamalarla testler uydurmayın.
Test etmeyi bırakmak, güvenle refactor edemediğiniz kodları şeffaf hale getirmek için daha iyidir.

%100 kapsam olmadan da mükemmel testlere sahip olabilirsiniz.
Örneğin, test doubles yerleştirmek için kurucu metodundaki IF’ler gibi durumlar %100 kapsama ulaşmayı pratik olmaktan çıkarabilir.

İyi testler, aynı kod satırını farklı koşullar ve dallanmalarda defalarca test eder; böylece gerçek dışı olarak %100’den fazla kapsama bile ulaşabilirler.

### Test Sınıfları

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#test-sınıfları)

#### Yerel test sınıflarını amacına göre adlandırın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Sınıfları](#test-sınıfları) > [Bu bölüm](#yerel-test-sınıflarını-amacına-göre-adlandırın)

Yerel test sınıflarını ya hikayenin "when" kısmına göre adlandırın:

```ABAP
CLASS ltc_<public method name> DEFINITION FOR TESTING ... .
```

ya da hikayenin "given" kısmına göre:

```ABAP
CLASS ltc_<common setup semantics> DEFINITION FOR TESTING ... .
```

```ABAP
" anti-patternler
CLASS ltc_fra_online_detection_api DEFINITION FOR TESTING ... . " Test edilen sınıf zaten belli — neden tekrarlayasınız?
CLASS ltc_test DEFINITION FOR TESTING ....                      " Tabii ki test, başka ne olabilir ki?
```

#### Testleri yerel sınıflarda tutun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Sınıfları](#test-sınıfları) > [Bu bölüm](#testleri-yerel-sınıflarda-tutun)

Testleri, test edilen sınıfın yerel test include’ına koyun.
Bu sayede, sınıf refaktör edilirken testler kolayca bulunur ve tüm ilgili testler tek tuşla çalıştırılabilir,
[Test sınıfları nasıl çalıştırılır](#test-sınıfları-nasıl-çalıştırılır) bölümünde anlatıldığı gibi.

Bileşen, entegrasyon ve sistem testlerini ise ayrı bir global sınıfın yerel test include’ına koyun.
Bu testler doğrudan tek bir test edilen sınıfa bağlı olmadığından, ilgili sınıflardan herhangi birine değil, ayrı bir sınıfa konmalıdır.
Bu global test sınıfını `FOR TESTING` ve `ABSTRACT` olarak işaretleyin; böylece üretim kodunda kazara referans verilmesi önlenir.
Testlerin başka sınıflarda olması, gözden kaçmalarına ve ilgili sınıflar refaktör edilirken testlerin unutulmasına yol açabilir.

Bu nedenle, test edilen nesneleri belgelemek için *test relations* kullanmak faydalıdır.
Aşağıdaki örnekte, `hiring_test` test sınıfı `recruting` veya `candidate` sınıfındayken `Shift-Ctrl-F12` (Windows) veya `Cmd-Shift-F12` (macOS) kısayolu ile çalıştırılabilir:

```abap
"! @testing recruting
"! @testing candidate
class hiring_test definition
  for testing risk level dangerous duration medium
  abstract.
  ...
endclass.
```


#### Yardımcı metodları yardım sınıflarında tutun

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Sınıfları](#test-sınıfları) > [Bu bölüm](#yardımcı-metodları-yardım-sınıflarında-tutun)

Birden fazla test sınıfı tarafından kullanılan yardımcı metodları bir yardımcı sınıfa koyun.
Yardımcı metodları kalıtım (is-a ilişkisi) veya delege etme (has-a ilişkisi) yoluyla erişilebilir yapın.

```abap
" kalıtım örneği

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

#### Test sınıfları nasıl çalıştırılır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Sınıfları](#test-sınıfları) > [Bu bölüm](#test-sınıfları-nasıl-çalıştırılır)

ABAP Development Tools’da testleri hızlıca çalıştırmak için şu klavye kısayollarını kullanın:

| Tuş Kombinasyonu     | İşlev                                                      |
| :------------------- | :--------------------------------------------------------- |
| `Ctrl`+`Shift`+`F9`  | Tüm testleri önizle (test ilişkileri dahil)                |
| `Ctrl`+`Shift`+`F10` | Bir sınıftaki tüm testleri çalıştır                        |
| `Ctrl`+`Shift`+`F11` | Testleri çalıştır ve kapsam ölçümlerini dahil et           |
| `Ctrl`+`Shift`+`F12` | Test ilişkisi olan diğer sınıflardaki testleri de çalıştır |

> macOS’te `Ctrl` yerine `Cmd` kullanın.

### Test Edilen Kod

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#test-edilen-kod)

#### Test edilen kodu anlamlı şekilde adlandırın, olmazsa CUT kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Edilen Kod](#test-edilen-kod) > [Bu bölüm](#test-edilen-kodu-anlamlı-şekilde-adlandırın-olmazsa-cut-kullanın)

Test edilen kodu temsil eden değişkene anlamlı bir isim verin:

```ABAP
DATA blog_post TYPE REF TO ...
```

Sadece sınıf adını, değersiz namespace ve önekleriyle tekrar etmeyin:

```ABAP
" anti-pattern
DATA clean_fra_blog_post TYPE REF TO ...
```

Farklı test kurulumlarınız varsa, nesnenin farklı durumlarını tanımlamak faydalı olabilir:

```ABAP
DATA empty_blog_post TYPE REF TO ...
DATA simple_blog_post TYPE REF TO ...
DATA very_long_blog_post TYPE REF TO ...
```

Anlamlı isim bulmakta zorlanırsanız, varsayılan olarak `cut` kullanabilirsiniz.
Kısaltma “code under test” (test edilen kod) anlamına gelir.

```ABAP
DATA cut TYPE REF TO ...
```

Özellikle karışık ve temiz olmayan testlerde, değişkeni `cut` olarak adlandırmak
okuyucunun gerçekte ne test edildiğini geçici olarak görmesini sağlar.
Ancak uzun vadede asıl çözüm, testleri temizlemektir.

#### Uygulamaya değil, arayüze karşı test yazın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Edilen Kod](#test-edilen-kod) > [Bu bölüm](#uygulamaya-değil-arayüze-karşı-test-yazın)

[_Private alanları değil public metodları test edin_](#private-alanları-değil-public-metodları-test-edin) maddesinin pratik sonucu olarak, test ettiğiniz kodu bir _arayüz_ (interface) ile tipleyin:

```ABAP
DATA code_under_test TYPE REF TO some_interface.
```

Doğrudan bir _sınıf_ ile tiplemek yerine:

```ABAP
" anti-pattern
DATA code_under_test TYPE REF TO some_class.
```

#### Test edilen kod çağrısını ayrı bir metoda çıkarın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Edilen Kod](#test-edilen-kod) > [Bu bölüm](#test-edilen-kod-çağrısını-ayrı-bir-metoda-çıkarın)

Eğer test edeceğiniz metod çok sayıda parametre veya hazırlık gerektiren veri alıyorsa, çağrıyı kendi içinde yardımcı bir metoda çıkarıp, ilginizi çekmeyen parametreler için varsayılan değerler atamak faydalı olur:

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

Orijinal metodu doğrudan çağırmak, testinizi anlamsız ayrıntılarla boğabilir:

```ABAP
" anti-pattern
DATA(itab) = cut->map_xml_to_itab( xml_string = '<xml></xml>'
                                   config     = VALUE #( 'some meaningless stuff' )
                                   format     = VALUE #( 'more meaningless stuff' ) ).
```

### Bağımlılık Enjeksiyonu (Injection)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#bağımlılık-enjeksiyonu-injection)

#### Test çiftlerini enjekte etmek için bağımlılık tersine çevrimini (Dependency Inversion) kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#test-çiftlerini-enjekte-etmek-için-bağımlılık-tersine-çevrimini-dependency-inversion-kullanın)

Dependency inversion demek, tüm bağımlılıkların constructor’a (yapıcı metoda) verilmesi demektir:

```ABAP
METHODS constructor
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD constructor.
  me->customizing_reader = customizing_reader.
ENDMETHOD.
```

Setter injection kullanmayın. Çünkü bu, üretim kodunun amaçlanmayan şekillerde kullanılmasına olanak verir:

```ABAP
" anti-pattern
METHODS set_customizing_reader
  IMPORTING
    customizing_reader TYPE REF TO if_fra_cust_obj_model_reader.

METHOD do_something.
  object->set_customizing_reader( a ).
  object->set_customizing_reader( b ). " biri bunu yapar mı dersiniz?
ENDMETHOD.
```

FRIENDS injection da kullanmayın. Bu yaklaşım, bağımlılıkları test double ile değiştirmeden önce üretim bağımlılıklarını yaratır, beklenmedik sorunlara yol açabilir. Ayrıca constructor’daki ilklemeleri atlar ve iç yapıların isim değişikliğinde kolayca bozulur.

```ABAP
" anti-pattern
METHOD setup.
  cut = NEW fra_my_class( ). " <- önce üretim bağımlılığını yaratır; bu test double ile bozulur
  cut->customizing_reader ?= cl_abap_testdouble=>create( 'if_fra_cust_obj_model_reader' ).
ENDMETHOD.

METHOD constructor.
  customizing_reader = fra_cust_obj_model_reader=>s_get_instance( ).
  customizing_reader->fill_buffer( ). " test double üzerinde çalışmaz, test edilemez.
ENDMETHOD.
```

#### ABAP Test Double aracını kullanmayı düşünün

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#abap-test-double-aracını-kullanmayı-düşünün)

```ABAP
DATA(customizing_reader) = CAST /clean/customizing_reader( cl_abap_testdouble=>create( '/clean/default_custom_reader' ) ).
cl_abap_testdouble=>configure_call( customizing_reader )->returning( sub_claim_customizing ).
customizing_reader->read( 'SOME_ID' ).
```

Özel test double’lara kıyasla daha kısa ve anlaşılır:

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

#### Test araçlarından faydalanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#test-araçlarından-faydalanın)

Genel olarak, temiz bir programlama stili sayesinde işinizin büyük bir kısmını standart ABAP birim testleri ve test double’ları ile yapabilirsiniz. Kullanılabilir framework’lerin örneklerle birlikte bir listesini [bu repository'de](https://github.com/SAP-samples/abap-test-isolation-examples) bulabilirsiniz. Bu depo, CleanABAP stil rehberine tamamen uyumlu olmayabilir; sadece test izolasyon araçlarına yardımcı olmak için örnekler sunmaktadır.

#### Test dikişlerini geçici çözüm olarak kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#test-dikişlerini-geçici-çözüm-olarak-kullanın)

Eğer diğer tüm teknikler başarısız olursa ya da eski kodun tehlikeli sığ sularında iseniz, test edilebilirliği sağlamak için [test seams](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptest-seam.htm) kullanmaktan kaçının.

İlk bakışta kullanışlı görünseler de, test dikişleri (seams) müdahaleci yapıdadır ve genellikle özel (private) bağımlılıklara dolanır. Bu nedenle uzun vadede stabil ve sürdürülebilir tutmak zordur.

Bu yüzden, test dikişleri yalnızca geçici bir çözüm olarak kullanmanızı; kodu daha test edilebilir bir hale getirmek için refaktoring yapana kadar bu yönteme başvurmanızı öneriyoruz.

#### Dependency-inverting constructor'a erişmek için LOCAL FRIENDS kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#dependency-inverting-constructora-erişmek-için-local-friends-kullanın)

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

#### LOCAL FRIENDS'i test edilen kodu ihlal etmek için kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#local-friendsi-test-edilen-kodu-ihlal-etmek-için-kullanmayın)

Mock veriyi yerleştirmek için private ve protected üyelere erişen birim testleri kırılgandır:
Test edilen kodun iç yapısı değiştiğinde bu testler bozulur.

```ABAP
" anti-pattern
CLASS /dirty/class_under_test DEFINITION LOCAL FRIENDS unit_tests.
CLASS unit_tests IMPLEMENTATION.
  METHOD returns_right_result.
    cut->some_private_member = 'AUNIT_DUMMY'.
  ENDMETHOD.
ENDCLASS.
```

#### Sadece test sırasında kullanılacak özellikleri production koduna eklemeyin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#sadece-test-sırasında-kullanılacak-özellikleri-production-koduna-eklemeyin)

[Test dikişlerini geçici çözüm olarak kullanın](#test-dikişlerini-geçici-çözüm-olarak-kullanın) başlığı altında daha önce açıklanan nedenlerle, otomatik testler sırasında kullanılmak üzere yalnızca üretim koduna eklenen herhangi bir şeyden kaçınılmalıdır.
```ABAP
" anti-pattern
IF is_unit_test_running = abap_true.
  "some logic here that runs only during unit tests
ENDIF.  
```
Son kullanıcı tarafından çalıştırılması amaçlanan test özelliklerinin (örneğin, simüle edilmiş fiş kaydı veya test modunda bir raporun çalıştırılması) uygulama alanının bir parçası olduğunu ve geçerli bir kullanım durumu olmaya devam ettiğini unutmayın.

#### Mock işlemleri için alt sınıf yaratmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#mock-işlemleri-için-alt-sınıf-yaratmayın)

Birim testlerinizde metodları taklit etmek (mock etmek) için alt sınıf oluşturup metodları geçersiz kılmayın (override yapmayın).
Bu yöntem çalışsa da kırılgandır; çünkü kod yeniden düzenlendiğinde testler kolayca bozulur.
Ayrıca, gerçek kullanıcıların sınıfınızdan kalıtım almasına izin verir ki, bu da [açıkça tasarlanmamışsa sizi zor durumda bırakabilir](#eğer-sınıf-kalıtım-inheritance-için-tasarlanmadıysa-final-olarak-işaretleyin).

```ABAP
" anti-pattern
CLASS unit_tests DEFINITION INHERITING FROM /dirty/real_class FOR TESTING [...].  
  PROTECTED SECTION.  
    METHODS needs_to_be_mocked REDEFINITION.  
```

Legacy kodu test edebilmek için
[test seams](#test-dikişlerini-geçici-çözüm-olarak-kullanın) yöntemine başvurun.
Test seams de kırılgan olabilir ama en azından üretim kodunun davranışını değiştirmezler;
oysa miras alınabilirliği sağlamak için `FINAL` ibaresini kaldırmak ya da metodun kapsamını `PRIVATE`'den `PROTECTED`'a değiştirmek üretim davranışını etkiler.

Yeni kod yazarken test edilebilirlik sorununu doğrudan sınıf tasarımında göz önünde bulundurun
ve farklı, daha iyi yöntemler bulun.
Yaygın iyi uygulamalar arasında
[test araçlarından faydalanmak](#test-araçlarından-faydalanın)
ve problemli metodu kendi arayüzü olan ayrı bir sınıfa çıkarmak yer alır.

> Bu,
> [otomatik testlerde kullanılmak üzere yalnızca üretim koduna ek özellikler eklemeyin](#sadece-test-sırasında-kullanılacak-özellikleri-production-koduna-eklemeyin)
> maddesinin daha spesifik bir versiyonudur.


#### Gereksiz şeyleri mock'lamayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#gereksiz-şeyleri-mocklamayın)

```ABAP
cut = NEW /clean/class_under_test( db_reader = db_reader
                                   config    = VALUE #( )
                                   writer    = VALUE #( ) ).
```

Verilen (given) değerlerinizi mümkün olduğunca hassas tanımlayın: testinizin ihtiyacı olmayan verileri ayarlamayın,
ve çağrılmayan nesneleri taklit (mock) etmeyin.
Bunlar, okuyucunun gerçek olanı anlamasını zorlaştırır.

```ABAP
" anti-pattern
cut = NEW /dirty/class_under_test( db_reader = db_reader
                                   config    = config
                                   writer    = writer ).
```

Ayrıca, bazen bir şeyi taklit etmek hiç gerekmez –
genellikle veri yapıları ve veri konteynerleri için geçerlidir.
Örneğin, `transient_log` gibi yan etkisi olmayan sadece veri tutan nesneler için,
birim testleriniz üretim versiyonuyla da rahatlıkla çalışabilir.

#### Kendi test framework'ünüzü yazmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bağımlılık Enjeksiyonu (Injection)](#bağımlılık-enjeksiyonu-injection) > [Bu bölüm](#kendi-test-frameworkünüzü-yazmayın)

Birim testler — entegrasyon testlerinin aksine — veriyi girdi olarak alıp çıktıyı üretmeli; tüm test verileri ihtiyaç duyuldukça anında tanımlanmalıdır.

```ABAP
cl_abap_testdouble=>configure_call( test_double )->returning( data ).
```

"*Test case ID*" gibi farklı senaryoları ayırt eden kendi framework'lerinizi geliştirmeye başlamayın.
Ortaya çıkan kod çok uzun ve karışık olur, uzun vadede bu testleri sürdürebilmeniz zorlaşır.

```ABAP
" anti-pattern

test_double->set_test_case( 1 ).

CASE test_case.
  WHEN 1.
  WHEN 2.
ENDCASE.
```

### Test Metodları

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#test-metodları)

#### Test metodu isimleri: verilen ve bekleneni yansıtsın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Metodları](#test-metodları) > [Bu bölüm](#test-metodu-isimleri-verilen-ve-bekleneni-yansıtsın)

İyi isimler, testin given (verilen durum) ve then (beklenen sonuç) kısımlarını yansıtır:

```ABAP
METHOD reads_existing_entry.
METHOD throws_on_invalid_key.
METHOD detects_invalid_input.
```

Kötü isimler ise when (tetikleyici olay) kısmını yansıtır, anlamsız tekrarlar içerir ya da şifreli (anlaşılmaz) olur:

```ABAP
" anti-patterns

" Beklenen ne, başarı mı yoksa hata mı?
METHOD get_conversion_exits.

" Test metodu olduğu zaten belli, başka ne yapmalı ki?
METHOD test_loop.

" Parametreli ama amacı ne belli değil?
METHOD parameterized_test.

" "_wo_w" ne anlama geliyor? Bir yıl sonra bunu hatırlayacak mısın?
METHOD get_attributes_wo_w.
```

ABAP metod isimlerinde en fazla 30 karakter kullanılabildiği için,
isim yeterince anlamlı değilse açıklayıcı bir yorum eklemek mantıklıdır.
ABAP Doc ya da test metodunun ilk satırı bu amaç için uygun olabilir.

Çok uzun isimli çok sayıda test metodunuz varsa, bu durum
test sınıfınızı parçalara bölüp
farklı given durumlarını sınıf isimlerinde ifade etmeniz gerektiğine işaret edebilir.

#### Given-When-Then yapısını kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Metodları](#test-metodları) > [Bu bölüm](#given-when-then-yapısını-kullanın)

Test kodunuzu given-when-then paradigmasına göre düzenleyin:
Önce başlangıç hazırlıklarını yapın ("given"),
sonra test edilen fonksiyonu çağırın ("when"),
en sonunda sonucu doğrulayın ("then").

Eğer given ya da then bölümleri o kadar uzun olur ki
üç bölümü görsel olarak ayıramaz hale gelirseniz,
alt metodlar çıkararak bölümlendirin.

Boş satırlar veya yorumlar görünüşte iyi olsa da
gerçekten görsel karmaşayı azaltmazlar.
Yine de, bölümleri ayırmak için okuyucu ve test yazmaya yeni başlayanlar için faydalıdırlar.

#### "When" tam olarak bir çağrı olmalıdır

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Metodları](#test-metodları) > [Bu bölüm](#when-tam-olarak-bir-çağrı-olmalıdır)

Test metodunuzun "when" bölümü yalnızca sınıf altındaki (class under test) metoda tam olarak bir kez çağrı yaptığından emin olun:

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Birden fazla şey çağırmak, metodun net bir odağı olmadığını ve çok fazla şeyi test ettiğini gösterir.
Bu da test başarısız olduğunda hatanın nedenini bulmayı zorlaştırır:
başarısızlık ilk, ikinci mi yoksa üçüncü çağrıdan mı kaynaklandı?
Ayrıca okuyucuyu karıştırır çünkü hangi özelliğin test edildiği net değildir.

#### Gerçekten gerekmedikçe TEARDOWN kullanmayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Metodları](#test-metodları) > [Bu bölüm](#gerçekten-gerekmedikçe-teardown-kullanmayın)

`teardown` metodları genellikle sadece entegrasyon testlerinde veritabanı kayıtlarını veya diğer dış kaynakları temizlemek için gereklidir.

Test sınıfının üyelerini, özellikle `cut` ve kullanılan test double nesnelerini sıfırlamak gereksizdir;
çünkü bunlar bir sonraki test metodundan önce `setup` metodu tarafından zaten üzerine yazılır.

### Test Verisi (Test Data)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#test-verisi-test-data)

#### Anlamı fark etmek kolay olmalı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Verisi (Test Data)](#test-verisi-test-data) > [Bu bölüm](#anlamı-fark-etmek-kolay-olmalı)

Unit testlerde, hangi verilerin ve test double’ların önemli olduğunu hızlıca ayırt etmek istersiniz,
ve hangilerinin sadece kodun çökmesini engellemek için orada olduğunu kolayca anlamak istersiniz.
Bunu desteklemek için anlamı olmayan şeylere bariz isimler ve değerler verin, örneğin:

```ABAP
DATA(alert_id) = '42'.                             " iyi bilinen anlamsız sayılar
DATA(detection_object_type) = '?=/"&'.             " 'klavye kazaları'
CONSTANTS some_random_number TYPE i VALUE 782346.  " açıklayıcı değişken isimleri
```

İnsanları, gerçek nesnelere veya gerçek özelleştirmeye bağlandığına inandırmayın, eğer böyle bir bağlantı yoksa:

```ABAP
" anti-pattern
DATA(alert_id) = '00000001223678871'.        " bu uyarı gerçekten mevcut
DATA(detection_object_type) = 'FRA_SCLAIM'.  " bu tespit nesnesi tipi de öyle
CONSTANTS memory_limit TYPE i VALUE 4096.    " bu sayı özenle seçilmiş gibi görünüyor
```

#### Farklılıklar hemen göze çarpmalı

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Verisi (Test Data)](#test-verisi-test-data) > [Bu bölüm](#farklılıklar-hemen-göze-çarpmalı)

```ABAP
exp_parameter_in = VALUE #( ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END1' )
                            ( parameter_name = '45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789END2' ) ).
```

Uzun ve anlamsız stringleri karşılaştırmaya zorlayarak okuyucuyu küçük farkları yakalamaya mecbur bırakmayın.

#### Test verisinin amacı ve önemini tanımlamak için sabitler (constant) kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Test Verisi (Test Data)](#test-verisi-test-data) > [Bu bölüm](#test-verisinin-amacı-ve-önemini-tanımlamak-için-sabitler-constant-kullanın)

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

### Doğrulamalar (Assertions)

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Bu bölüm](#doğrulamalar-assertions)

#### Az ve odaklı doğrulamalar yapın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#az-ve-odaklı-doğrulamalar-yapın)

Test metodunun yalnızca o testle ilgili olan durumu doğrulamasını sağlayın ve bunu az sayıda assertion ile yapın.

```ABAP
METHOD rejects_invalid_input.
  " when
  DATA(is_valid) = cut->is_valid_input( 'SOME_RANDOM_ENTRY' ).
  " then
  cl_abap_unit_assert=>assert_false( is_valid ).
ENDMETHOD.
```

Çok fazla assertion yapmak, metodun net bir odağa sahip olmadığının göstergesidir.
Bu durum, üretim kodu ile test kodunun gereğinden fazla bağlanmasına yol açar; böylece bir özellik değiştiğinde, aslında doğrudan ilgili olmayan birçok testin de yeniden yazılması gerekir.
Ayrıca, çok sayıda farklı assertion okuyucuyu şaşırtır ve önemli, ayırt edici assertion’ın öne çıkmasını engeller.

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

#### Doğru ASSERT türünü kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#doğru-assert-türünü-kullanın)

```ABAP
cl_abap_unit_assert=>assert_equals( act = table
                                    exp = test_data ).
```

Assertionlar çoğu zaman göründüğünden daha fazlasını yapar; örneğin `assert_equals` hem tip uyumunu kontrol eder hem de değerler farklıysa ayrıntılı açıklamalar sunar.
Yanlış veya çok genel assertion’lar kullanmak, hata mesajından sorunun ne olduğunu görmenize engel olur ve sizi hemen debugger’a yönlendirir.

```ABAP
" anti-pattern
cl_abap_unit_assert=>assert_true( xsdbool( act = exp ) ).
```

#### Miktarı değil içeriği doğrulayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#miktarı-değil-içeriği-doğrulayın)

```ABAP
assert_contains_exactly( actual   = table
                         expected = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

Sihirli sayı (magic number) ile miktar kontrolü yapan assertion’lar yazmaktan kaçının; beklediğiniz gerçek içeriği ifade etmek çok daha iyidir.
Çünkü sayılar değişse bile beklentiler karşılanıyor olabilir. Öte yandan, sayılar eşleşse bile içerik tamamen beklenmedik olabilir.

```ABAP
" anti-pattern
assert_equals( act = lines( log_messages )
               exp = 3 ).
```

#### İçeriği değil kaliteyi doğrulayın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#içeriği-değil-kaliteyi-doğrulayın)

Eğer sonuçların gerçek içeriğiyle değil de meta kalitesiyle ilgileniyorsanız, bunu uygun bir assert ile ifade edin:

```ABAP
assert_all_lines_shorter_than( actual_lines        = table
                               expected_max_length = 80 ).
```

Kesin içeriği kontrol etmek, aslında neyi test etmek istediğinizi gizler.
Ayrıca kırılgan olur çünkü refaktoring sonucu farklı ama tamamen kabul edilebilir bir sonuç ortaya çıkabilir; bu da çok hassas unit testlerinizi bozar.

```ABAP
" anti-pattern
assert_equals( act = table
               exp = VALUE string_table( ( `ABC` ) ( `DEF` ) ( `GHI` ) ) ).
```

#### Beklenen exception'lar için FAIL kullanın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#beklenen-exceptionlar-için-fail-kullanın)

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

#### Beklenmeyen exception'ları yakalayıp FAIL demek yerine ilerletin

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#beklenmeyen-exceptionları-yakalayıp-fail-demek-yerine-ilerletin)

```ABAP
METHODS reads_entry FOR TESTING RAISING /clean/some_exception.

METHOD reads_entry.
  "when
  DATA(entry) = cut->read_something( ).
  "then
  cl_abap_unit_assert=>assert_not_initial( entry ).
ENDMETHOD.
```

Test kodunuz odak noktasını "happy path" yani beklenen ve olumlu senaryo üzerinde tutar ve bu nedenle şu tip kodlara kıyasla çok daha okunabilir ve anlaşılır olur:

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

#### Kodunuzu kısaltmak ve tekrarları önlemek için özel ASSERT'lar yazın

> [Clean ABAP](#clean-abap) > [İçerik](#içerik) > [Test Etme](#test-etme) > [Doğrulamalar (Assertions)](#doğrulamalar-assertions) > [Bu bölüm](#kodunuzu-kısaltmak-ve-tekrarları-önlemek-için-özel-assertlar-yazın)

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

Bunu defalarca kopyala-yapıştır yapmak yerine.
