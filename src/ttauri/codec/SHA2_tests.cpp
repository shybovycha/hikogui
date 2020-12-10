// Copyright 2020 Pokitec
// All rights reserved.

#include "ttauri/codec/SHA2.hpp"
#include "ttauri/codec/base_n.hpp"
#include "ttauri/required.hpp"
#include "ttauri/strings.hpp"
#include <gtest/gtest.h>
#include <iostream>

using namespace std;
using namespace tt;

template<typename T>
std::string test_sha2(bstring value)
{
    auto hash = T();
    hash.add(value);

    return base16::encode(hash.get_bytes());
}

template<typename T>
std::string test_sha2(std::string value)
{
    return test_sha2<T>(to_bstring(value));
}

#define ASSERT_CASEEQ(a, b) ASSERT_EQ(to_lower(a), to_lower(b))

TEST(SHA2, Empty) {
    ASSERT_CASEEQ(test_sha2<SHA224>(""), "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f");
    ASSERT_CASEEQ(test_sha2<SHA256>(""), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    ASSERT_CASEEQ(test_sha2<SHA384>(""), "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b");
    ASSERT_CASEEQ(test_sha2<SHA512>(""), "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e");
    ASSERT_CASEEQ(test_sha2<SHA512_224>(""), "6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4");
    ASSERT_CASEEQ(test_sha2<SHA512_256>(""), "c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a");
}

TEST(SHA2, NESSIE256Set1) {
    ASSERT_CASEEQ(
        test_sha2<SHA256>(""),
        "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("a"),
        "CA978112CA1BBDCAFAC231B39A23DC4DA786EFF8147C4E72B9807785AFEE48BB");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("abc"),
        "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("message digest"),
        "F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("abcdefghijklmnopqrstuvwxyz"),
        "71C480DF93D6AE2F1EFAD1447C66C9525E316218CF51FC8D9ED832F2DAF18B73");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
        "248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
        "DB4BFCBD4DA0CD85A60C3C37D3FBD8805C77F15FC6B1FDFE614EE0A7C8FDB4C0");

    ASSERT_CASEEQ(
        test_sha2<SHA256>("12345678901234567890123456789012345678901234567890123456789012345678901234567890"),
        "F371BC4A311F2B009EEF952DD83CA80E2B60026C8E935592D0F9C308453C813E");

    ASSERT_CASEEQ(
        test_sha2<SHA256>(std::string(1'000'000, 'a')),
        "CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0");

    // Same test, but with chunks of 10 characters.
    auto h = SHA256();
    for (int i = 0; i != 99'999; i++) {
        h.add(to_bstring("aaaaaaaaaa"), false);
    }
    h.add(to_bstring("aaaaaaaaaa"));
    ASSERT_CASEEQ(
        base16::encode(h.get_bytes()),
        "CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0");
}

TEST(SHA2, NESSIE512Set1) {
    ASSERT_CASEEQ(
        test_sha2<SHA512>(""),
        "CF83E1357EEFB8BDF1542850D66D8007"
        "D620E4050B5715DC83F4A921D36CE9CE"
        "47D0D13C5D85F2B0FF8318D2877EEC2F"
        "63B931BD47417A81A538327AF927DA3E");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("a"),
        "1F40FC92DA241694750979EE6CF582F2"
        "D5D7D28E18335DE05ABC54D0560E0F53"
        "02860C652BF08D560252AA5E74210546"
        "F369FBBBCE8C12CFC7957B2652FE9A75");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("abc"), 
        "DDAF35A193617ABACC417349AE204131"
        "12E6FA4E89A97EA20A9EEEE64B55D39A"
        "2192992A274FC1A836BA3C23A3FEEBBD"
        "454D4423643CE80E2A9AC94FA54CA49F");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("message digest"),
        "107DBF389D9E9F71A3A95F6C055B9251"
        "BC5268C2BE16D6C13492EA45B0199F33"
        "09E16455AB1E96118E8A905D5597B720"
        "38DDB372A89826046DE66687BB420E7C");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("abcdefghijklmnopqrstuvwxyz"),
        "4DBFF86CC2CA1BAE1E16468A05CB9881"
        "C97F1753BCE3619034898FAA1AABE429"
        "955A1BF8EC483D7421FE3C1646613A59"
        "ED5441FB0F321389F77F48A879C7B1F1");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
        "204A8FC6DDA82F0A0CED7BEB8E08A416"
        "57C16EF468B228A8279BE331A703C335"
        "96FD15C13B1B07F9AA1D3BEA57789CA0"
        "31AD85C7A71DD70354EC631238CA3445");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
        "1E07BE23C26A86EA37EA810C8EC78093"
        "52515A970E9253C26F536CFC7A9996C4"
        "5C8370583E0A78FA4A90041D71A4CEAB"
        "7423F19C71B9D5A3E01249F0BEBD5894");

    ASSERT_CASEEQ(
        test_sha2<SHA512>("12345678901234567890123456789012345678901234567890123456789012345678901234567890"),
        "72EC1EF1124A45B047E8B7C75A932195"
        "135BB61DE24EC0D1914042246E0AEC3A"
        "2354E093D76F3048B456764346900CB1"
        "30D2A4FD5DD16ABB5E30BCB850DEE843");

    ASSERT_CASEEQ(
        test_sha2<SHA512>(std::string(1'000'000, 'a')),
        "E718483D0CE769644E2E42C7BC15B463"
        "8E1F98B13B2044285632A803AFA973EB"
        "DE0FF244877EA60A4CB0432CE577C31B"
        "EB009C5C2C49AA2E4EADB217AD8CC09B");

    // Same test, but with chunks of 10 characters.
    auto h = SHA512();
    for (int i = 0; i != 99'999; i++) {
        h.add(to_bstring("aaaaaaaaaa"), false);
    }
    h.add(to_bstring("aaaaaaaaaa"));
    ASSERT_CASEEQ(
        base16::encode(h.get_bytes()),
        "E718483D0CE769644E2E42C7BC15B463"
        "8E1F98B13B2044285632A803AFA973EB"
        "DE0FF244877EA60A4CB0432CE577C31B"
        "EB009C5C2C49AA2E4EADB217AD8CC09B");
}
