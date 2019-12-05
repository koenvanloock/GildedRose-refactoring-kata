package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def updateQuality(): Unit = items.foreach { currentItem =>
    currentItem match {
      case a if isRedularItem(a) => handleRegularItem(a)
      case b if b.name == "Aged Brie" => handleBrie(b)
      case c if c.name == "Backstage passes to a TAFKAL80ETC concert" => handleTicket(c)
      case d if isConjured(d) => handleConjured(d)
      case _ => ()
    }

    updateSellIn(currentItem)
  }

  private def updateSellIn(currentItem: Item) = {
    if (isNonLegendary(currentItem)) {
      currentItem.sellIn = currentItem.sellIn - 1
    }
  }

  private def handleTicket(currentItem: Item) = {
    if (currentItem.sellIn > 10) {
      setQuality(currentItem, 1)
    } else if (currentItem.sellIn > 6) {
      setQuality(currentItem, 2)
    } else if (currentItem.sellIn > 0) {
      setQuality(currentItem, 3)
    } else {
      setQuality(currentItem, -currentItem.quality)
    }
  }

  private def handleBrie(currentItem: Item) = {
    val brieIncrease = if (currentItem.sellIn > 0) 1 else 2
    setQuality(currentItem, brieIncrease)
  }

  private def handleConjured(currentItem: Item) = {
    val conjuredDecrease = if (currentItem.sellIn > 0) -2 else -4
    setQuality(currentItem, conjuredDecrease)
  }

  private def handleRegularItem(currentItem: Item) = {
    val regularDecrease = if (currentItem.sellIn > 0) -1 else -2
    setQuality(currentItem, regularDecrease)
  }

  private def setQuality(item: Item, increase: Int): Unit = item.quality + increase match {
    case a if a < 0 => item.quality = 0
    case b if b > 50 => item.quality = 50
    case c => item.quality = c
  }

  private def isRedularItem(item: Item) =
    item.name != "Aged Brie" &&
      item.name != "Backstage passes to a TAFKAL80ETC concert" &&
      item.name != "Sulfuras, Hand of Ragnaros" &&
      !isConjured(item)

  private def isConjured(item: Item) = item.name.toLowerCase.contains("conjured")

  private def isNonLegendary(item: Item) = item.name != "Sulfuras, Hand of Ragnaros"
}