import XCTest
import SwiftTreeSitter
import TreeSitterRoto

final class TreeSitterRotoTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_roto())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Roto grammar")
    }
}
