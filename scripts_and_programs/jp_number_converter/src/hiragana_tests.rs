#[cfg(test)]
use crate::convert_number;

#[test]
pub fn hiragana_tests() {
    assert_eq!(convert_number("1"), "いち");
    assert_eq!(convert_number("2"), "に");
    assert_eq!(convert_number("3"), "さん");
    assert_eq!(convert_number("4"), "よん");
    assert_eq!(convert_number("5"), "ご");
    assert_eq!(convert_number("6"), "ろく");
    assert_eq!(convert_number("7"), "なな");
    assert_eq!(convert_number("8"), "はち");
    assert_eq!(convert_number("9"), "きゅう");
    assert_eq!(convert_number("10"), "じゅう");
    assert_eq!(convert_number("22"), "にじゅうに");
    assert_eq!(convert_number("33"), "さんじゅうさん");
    assert_eq!(convert_number("44"), "よんじゅうよん");
    assert_eq!(convert_number("55"), "ごじゅうご");
    assert_eq!(convert_number("66"), "ろくじゅうろく");
    assert_eq!(convert_number("77"), "ななじゅうなな");
    assert_eq!(convert_number("88"), "はちじゅうはち");
    assert_eq!(convert_number("99"), "きゅうじゅうきゅう");
    assert_eq!(convert_number("100"), "ひゃく");
    assert_eq!(convert_number("222"), "にひゃくにじゅうに");
    assert_eq!(convert_number("333"), "さんびゃくさんじゅうさん");
    assert_eq!(convert_number("444"), "よんひゃくよんじゅうよん");
    assert_eq!(convert_number("555"), "ごひゃくごじゅうご");
    assert_eq!(convert_number("666"), "ろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("777"), "ななひゃくななじゅうなな");
    assert_eq!(convert_number("888"), "はっぴゃくはちじゅうはち");
    assert_eq!(convert_number("999"), "きゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("1000"), "せん");
    assert_eq!(convert_number("2222"), "にせんにひゃくにじゅうに");
    assert_eq!(convert_number("3333"), "さんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("4444"), "よんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("5555"), "ごせんごひゃくごじゅうご");
    assert_eq!(convert_number("6666"), "ろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("7777"), "ななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("8888"), "はっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("9999"), "きゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("10000"), "いちまん");
    assert_eq!(convert_number("22222"), "にまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("33333"), "さんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("44444"), "よんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("55555"), "ごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("66666"), "ろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("77777"), "ななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("88888"), "はちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("99999"), "きゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("100000"), "じゅうまん");
    assert_eq!(convert_number("222222"), "にじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("333333"), "さんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("444444"), "よんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("555555"), "ごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("666666"), "ろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("777777"), "ななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("888888"), "はちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("999999"), "きゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("1000000"), "ひゃくまん");
    assert_eq!(convert_number("2222222"), "にひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("3333333"), "さんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("4444444"), "よんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("5555555"), "ごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("6666666"), "ろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("7777777"), "ななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("8888888"), "はっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("9999999"), "きゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("10000000"), "いっせんまん");
    assert_eq!(convert_number("22222222"), "にせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("33333333"), "さんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("44444444"), "よんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("55555555"), "ごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("66666666"), "ろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("77777777"), "ななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("88888888"), "はっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("99999999"), "きゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("100000000"), "いちおく");
    assert_eq!(convert_number("222222222"), "におくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("333333333"), "さんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("444444444"), "よんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("555555555"), "ごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("666666666"), "ろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("777777777"), "ななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("888888888"), "はちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("999999999"), "きゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("1000000000"), "じゅうおく");
    assert_eq!(convert_number("2222222222"), "にじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("3333333333"), "さんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("4444444444"), "よんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("5555555555"), "ごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("6666666666"), "ろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("7777777777"), "ななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("8888888888"), "はちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("9999999999"), "きゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("10000000000"), "ひゃくおく");
    assert_eq!(convert_number("22222222222"), "にひゃくにじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("33333333333"), "さんびゃくさんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("44444444444"), "よんひゃくよんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("55555555555"), "ごひゃくごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("66666666666"), "ろっぴゃくろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("77777777777"), "ななひゃくななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("88888888888"), "はっぴゃくはちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("99999999999"), "きゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("100000000000"), "いっせんおく");
    assert_eq!(convert_number("222222222222"), "にせんにひゃくにじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("333333333333"), "さんぜんさんびゃくさんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("444444444444"), "よんせんよんひゃくよんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("555555555555"), "ごせんごひゃくごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("666666666666"), "ろくせんろっぴゃくろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("777777777777"), "ななせんななひゃくななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("888888888888"), "はっせんはっぴゃくはちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("999999999999"), "きゅうせんきゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("1000000000000"), "いっちょう");
    assert_eq!(convert_number("2222222222222"), "にちょうにせんにひゃくにじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("3333333333333"), "さんちょうさんぜんさんびゃくさんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("4444444444444"), "よんちょうよんせんよんひゃくよんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("5555555555555"), "ごちょうごせんごひゃくごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("6666666666666"), "ろくちょうろくせんろっぴゃくろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("7777777777777"), "ななちょうななせんななひゃくななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("8888888888888"), "はちちょうはっせんはっぴゃくはちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("9999999999999"), "きゅうちょうきゅうせんきゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("10000000000000"), "じゅうちょう");
    assert_eq!(convert_number("22222222222222"), "にじゅうにちょうにせんにひゃくにじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("33333333333333"), "さんじゅうさんちょうさんぜんさんびゃくさんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("44444444444444"), "よんじゅうよんちょうよんせんよんひゃくよんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("55555555555555"), "ごじゅうごちょうごせんごひゃくごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("66666666666666"), "ろくじゅうろくちょうろくせんろっぴゃくろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("77777777777777"), "ななじゅうななちょうななせんななひゃくななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("88888888888888"), "はちじゅうはちちょうはっせんはっぴゃくはちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("99999999999999"), "きゅうじゅうきゅうちょうきゅうせんきゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("100000000000000"), "ひゃくちょう");
    assert_eq!(convert_number("222222222222222"), "にひゃくにじゅうにちょうにせんにひゃくにじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("333333333333333"), "さんびゃくさんじゅうさんちょうさんぜんさんびゃくさんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("444444444444444"), "よんひゃくよんじゅうよんちょうよんせんよんひゃくよんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("555555555555555"), "ごひゃくごじゅうごちょうごせんごひゃくごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("666666666666666"), "ろっぴゃくろくじゅうろくちょうろくせんろっぴゃくろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("777777777777777"), "ななひゃくななじゅうななちょうななせんななひゃくななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("888888888888888"), "はっぴゃくはちじゅうはちちょうはっせんはっぴゃくはちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("999999999999999"), "きゅうひゃくきゅうじゅうきゅうちょうきゅうせんきゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
    assert_eq!(convert_number("1000000000000000"), "いっせんちょう");
    assert_eq!(convert_number("2222222222222222"), "にせんにひゃくにじゅうにちょうにせんにひゃくにじゅうにおくにせんにひゃくにじゅうにまんにせんにひゃくにじゅうに");
    assert_eq!(convert_number("3333333333333333"), "さんぜんさんびゃくさんじゅうさんちょうさんぜんさんびゃくさんじゅうさんおくさんぜんさんびゃくさんじゅうさんまんさんぜんさんびゃくさんじゅうさん");
    assert_eq!(convert_number("4444444444444444"), "よんせんよんひゃくよんじゅうよんちょうよんせんよんひゃくよんじゅうよんおくよんせんよんひゃくよんじゅうよんまんよんせんよんひゃくよんじゅうよん");
    assert_eq!(convert_number("5555555555555555"), "ごせんごひゃくごじゅうごちょうごせんごひゃくごじゅうごおくごせんごひゃくごじゅうごまんごせんごひゃくごじゅうご");
    assert_eq!(convert_number("6666666666666666"), "ろくせんろっぴゃくろくじゅうろくちょうろくせんろっぴゃくろくじゅうろくおくろくせんろっぴゃくろくじゅうろくまんろくせんろっぴゃくろくじゅうろく");
    assert_eq!(convert_number("7777777777777777"), "ななせんななひゃくななじゅうななちょうななせんななひゃくななじゅうななおくななせんななひゃくななじゅうななまんななせんななひゃくななじゅうなな");
    assert_eq!(convert_number("8888888888888888"), "はっせんはっぴゃくはちじゅうはちちょうはっせんはっぴゃくはちじゅうはちおくはっせんはっぴゃくはちじゅうはちまんはっせんはっぴゃくはちじゅうはち");
    assert_eq!(convert_number("9999999999999999"), "きゅうせんきゅうひゃくきゅうじゅうきゅうちょうきゅうせんきゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう");
}