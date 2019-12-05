package com.gildedrose

import org.scalatest._

class GildedRoseTest extends FlatSpec with Matchers {
  it should "return the first item if it is the only item" in {
    var items = Array[Item](new Item("foo", 0, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    (app.items(0).name) should equal("foo")
  }

  it should "lower the quality and sellin for a nonlegendary nonconjured nonbrie nonticket item with both values lager than 0" in {
    var items = Array[Item](new Item("foo", 1, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 4
    items(0).sellIn shouldBe 0
  }

  it should "lower the quality and sellin for a nonlegendary nonconjured nonbrie nonticket item with 2 when quality larger than 0 and the selin is 0" in {
    var items = Array[Item](new Item("foo", 0, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 3
    items(0).sellIn shouldBe -1
  }

  it should "not lower the quality if it is 0" in {
    var items = Array[Item](new Item("foo", 0, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 0
    items(0).sellIn shouldBe -1
  }

  it should "increase aged Brie in quality" in {
    var items = Array[Item](new Item("Aged Brie", 8, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 6
    items(0).sellIn shouldBe 7
  }

  it should "increase aged Brie by 2 quality when it's date has passed" in {
    var items = Array[Item](new Item("Aged Brie", 0, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 7
    items(0).sellIn shouldBe -1
  }

  it should "increase aged Brie quality up to 50 but no more" in {
    var items = Array[Item](new Item("Aged Brie", 0, 49))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 50
    items(0).sellIn shouldBe -1
    app.updateQuality()
    items(0).quality shouldBe 50
    items(0).sellIn shouldBe -2
  }

  it should "increase Backstage passes up to 50 but no more" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 1, 49))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 50
    items(0).sellIn shouldBe 0
  }

  it should "ignore Sulfuras" in {
    var items = Array(new Item("Sulfuras, Hand of Ragnaros", -5, 60))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 60
    items(0).sellIn shouldBe -5
  }

  it should "ignore Sulfuras even if its quality falls in bounds" in {
    var items = Array(new Item("Sulfuras, Hand of Ragnaros", -5, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 40
    items(0).sellIn shouldBe -5
  }

  it should "increase quality by 1 when 15 days are left" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 15, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 6
    items(0).sellIn shouldBe 14
  }

  it should "increase quality by 2 when exactly 10 days are left" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 10, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 7
    items(0).sellIn shouldBe 9
  }

  it should "increase quality by 2 when exactly 9 days are left" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 9, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 7
    items(0).sellIn shouldBe 8
  }

  it should "increase quality by 3 when 5 days are left" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 5, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 8
    items(0).sellIn shouldBe 4
  }

  it should "increase quality by 3 when 1 days are left" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 1, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 8
    items(0).sellIn shouldBe 0
  }

  it should "drop the value of a ticket to 0 when the seling is 0 or less" in {
    var items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 0, 50))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 0
    items(0).sellIn shouldBe -1
  }

  it should "drop the value of a Conjured item by 2 when its sellIn is 5" in {
    var items = Array[Item](new Item("Conjured", 5, 8))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 6
    items(0).sellIn shouldBe 4
  }

  it should "drop the value of a Conjured item by 4 when its sellIn is -1" in {
    var items = Array[Item](new Item("Conjured", 5, 8))
    val app = new GildedRose(items)
    app.updateQuality()
    items(0).quality shouldBe 6
    items(0).sellIn shouldBe 4
  }
}