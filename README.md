# ZipCoder - Powerful zip-code validation

![Haskell](https://img.shields.io/badge/Haskell-purple?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal](https://img.shields.io/badge/Cabal-blue?style=for-the-badge&logo=haskell&logoColor=white)

Powerful zip-code validation in a convenient CLI tool, written in Haskell.

Currently supporting zip-codes of a few dozen countries. PRs are welcome and it's a very easy change.

**Validating Dutch (NL) zip-codes**

Running:
```
λ cabal run zipcoder -- --country-code NL --zip-codes 1445KJ:1976HG
```

Output:
```
λ ZipCoder - Powerful zip-code validation
λ Starting ZipCoder after successful parsing of options..

λ Initiating validation for country NL..

λ All zip-codes are valid!
```

**Validating Japan (JP) zip-codes**

Running:
```
λ cabal run zipcoder -- --country-code JP --zip-codes 000-1234:000-34555
```

Output:
```
λ ZipCoder - Powerful zip-code validation
λ Starting ZipCoder after successful parsing of options..

λ Initiating validation for country JP..

[ERROR] Detected bad data! Bad entries will be printed below:

["000-34555"]
```