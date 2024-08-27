package Audio

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.SourceDataLine

object AudioConsts {
  val sampleRate = 32 * 1000
  val bitDepth = 8
}
object AudioSynth {
  def mkDataLine(sampleRate: Int, bitDepth: Int): SourceDataLine  = {
    val af : AudioFormat = new AudioFormat(sampleRate.toFloat, bitDepth, 1, true, true)
    val line : SourceDataLine = AudioSystem.getSourceDataLine(af)
    line.open(af, sampleRate)
    line.start()
    line
  }
  def stopDataLine(line: SourceDataLine) = {
    line.drain()
    line.stop()
  }
  def mkAudioSynth(sampleRate: Int, bitDepth: Int) = {
    AudioSynth(mkDataLine(sampleRate, bitDepth), sampleRate, bitDepth)
  }
  def withDataLine(sampleRate: Int, bitDepth: Int)(fn: SourceDataLine => Unit): Unit = {
    val line = mkDataLine(sampleRate, bitDepth)
    fn(line)
    stopDataLine(line)
  }
  def withAudioSynth(sampleRate: Int, bitDepth: Int)(fn: AudioSynth => Unit): Unit = {
    withDataLine(sampleRate, bitDepth) { line =>
      val as = AudioSynth(line, sampleRate, bitDepth)
      fn(as)
    }
  }
}
case class AudioSynth(line: SourceDataLine, sampleRate: Int, bitDepth: Int) {
  import Math._
  val maxVol = Math.pow(2.0, bitDepth - 1) - 1
  println(s"Initializing audio synth to sample rate : $sampleRate, bit depth: $bitDepth, max vol: $maxVol")
  def sine(phaseAngle: Double) : Double = sin(phaseAngle) * maxVol
  def sine3(phaseAngle : Double) : Double = {
    val f1 = sin(phaseAngle)
    val f3 = sin(phaseAngle * 3)
    (f1 * maxVol * 0.7) + (f3 * maxVol * 0.3)
  }
  def createSineWaveBuffer(freq: Double, lenMs: Int) = {
    val period = sampleRate / freq
    val noOfSamples = (lenMs * sampleRate) / 1000

    (0 until noOfSamples).map { i =>
      val phaseAngle = 2.0 * Math.PI * i / period
      sine(phaseAngle).toByte
    }.toArray
  }
  def createWaveBuffer(freq: Double, lenMs: Int, sineF : Double => Double = sine3) = {
    val period = sampleRate / freq
    val noOfSamples = (lenMs * sampleRate) / 1000

    (0 until noOfSamples).map { i =>
      val phaseAngle = 2.0 * Math.PI * i / period

      sineF(phaseAngle).toByte
    }.toArray
  }
  def createNoiseBuffer(lenMs: Int) = {
    val rand = new scala.util.Random
    val noOfSamples = (lenMs * sampleRate) / 1000

    (0 until noOfSamples).map { i =>
      (rand.nextDouble() * 127f).toByte
    }.toArray
  }
  def drain(): Unit = line.drain()
  def tone(freq: Int, lenMs: Int): Unit = {
    val audioBuffer = createSineWaveBuffer(freq, lenMs)
    line.write(audioBuffer, 0, audioBuffer.length)
  }
  def tone3(freq: Int, lenMs: Int): Unit = {
    val audioBuffer = createWaveBuffer(freq, lenMs)
    line.write(audioBuffer, 0, audioBuffer.length)
  }
  def sweep(f1: Int, f2: Int, steps: Int, lenMs: Int) : Unit = {
    val dur = lenMs / ((f2 - f1) / steps)
    (f1 to f2).by(steps).foreach { freq =>
      val audioBuffer = createSineWaveBuffer(freq, dur)
      line.write(audioBuffer, 0, audioBuffer.length)
    }
  }
  def noise(lenMs: Int) : Unit = {
    val audioBuffer = createNoiseBuffer(lenMs)
    line.write(audioBuffer, 0, audioBuffer.length)
  }
  def blip(f1: Int, f2: Int, steps: Int, lenMs: Int): Unit = {
    val durMs = lenMs / steps
    val audioBuffer1 = createSineWaveBuffer(f1, durMs)
    val audioBuffer2 = createSineWaveBuffer(f2, durMs)
    (1 to steps).foreach { _ =>
      line.write(audioBuffer1, 0, audioBuffer1.length)
      line.drain
      line.write(audioBuffer2, 0, audioBuffer2.length)
      line.drain
    }
  }
  def randomTones(f1: Int, f2: Int, steps: Int, lenMs: Int) : Unit = {
    val dur = lenMs / steps
    val rand = new scala.util.Random
    (1 to steps).foreach { _ =>
      val freq = rand.nextDouble() * (f2-f1) + f1
      val audioBuffer = createSineWaveBuffer(freq.toInt, dur)
      line.write(audioBuffer, 0, audioBuffer.length)
    }
  }
  def stop() = AudioSynth.stopDataLine(line)
}
