
/*
#include <HIDSerial.h>
#include <oddebug.h>
#include <usbconfig-prototype.h>
#include <usbconfig.h>
#include <usbdrv.h>
#include <usbportability.h>
#include <Matrix.h>
*/
/*

    ROOKIE - Chess playing robot
    version 0.0
    Authors: Matthew Kunkle, Galen Shick


*/

// Define Arduino Digital Pins (pins unused as of now: 0, 1, a1, a2, a3 )
// Note: Analog pins can be used as digital

//Matrix myMatrix(A4, A5); // To be removed

#define dir1 4        // Stepper 1 DIR+
#define step1 5       // Stepper 1 PUL+
#define dir2 6        // Stepper 2 DIR+
#define step2 8       // Stepper 2 PUL+
#define killswitch 3  // Interrupt can only be on pin 2 or 3 (pin 2 is USB)
#define latchPin 12   // Pin 1 on DM74165
#define clockPin 11   // Pin 2 on DM74165
#define dataPin 9     // Pin 9 on DM74165
#define ce 10         // Pin 15 on DM74165
#define counter93 A0  // Pin 15 on 74LS93
#define reset93 13    // Pin 2 on 74LS93 -Active HIGH
#define dplus 2       // USB D+ (Green)
#define dminus 7      // USB D- (White)


byte board[8];
const int spaceWidth = 50; // 50mm squares
const int stepsPerSpace = (spaceWidth / 8) * 200; // revs * steps per rev (1.8 deg/phase, 2mm pitch, 8mm per revolution)
const int wait = 500; // Delay for stepper steps

void setup() {

  Serial.begin(9600); // Will swap to new USB

  // PIN DECLARATIONS -----------------------------
  pinMode(step1, OUTPUT);
  pinMode(dir1, OUTPUT);
  pinMode(step2, OUTPUT);
  pinMode(dir2, OUTPUT);
  pinMode(latchPin, OUTPUT);
  pinMode(dataPin, INPUT);
  pinMode(clockPin, OUTPUT);
  pinMode(ce, OUTPUT);
  pinMode(counter93, OUTPUT);
  pinMode(reset93, OUTPUT);
  pinMode(killswitch, INPUT_PULLUP);
  // ---------------------------------------------

  digitalWrite(reset93, LOW);
  attachInterrupt(digitalPinToInterrupt(killswitch), kill, CHANGE); // If killswitch is triggered, kill() is called

}

void loop() {
  Serial.println("Reading");
  readBoard();
  delay(1000);
}


/*
   FUNCTION DESCRIPTION HERE
*/

void moveSpaces(int spaces1, int spaces2, int d1, int d2) {
  digitalWrite(dir1, d1);
  digitalWrite(dir2, d2);

  while (spaces1 > 0 && spaces2 > 0 ) { // move in both directions at the same time
    digitalWrite(step1, HIGH);
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    digitalWrite(step2, LOW);
    spaces1--;
    spaces2--;
  }
  while (spaces1 > 0) { // move the rest of the way in direction 1
    digitalWrite(step1, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    spaces1--;
  }
  while (spaces2 > 0) { // move the rest of the way in direction 2
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step2, LOW);
    spaces2--;
  }
}


/*
   FUNCTION DESCRIPTION HERE
*/

void Move(int wait, int dir) {
  if (wait <= 0) {
    return;
  }

  if (dir < 0 or dir >= 4) {
    return;
  }

  if (dir == 0) {
    // UP
    digitalWrite(dir1, HIGH);
    digitalWrite(step1, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    delayMicroseconds(wait);
  }
  else if (dir == 1) {
    // DOWN
    digitalWrite(dir1, LOW);
    digitalWrite(step1, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    delayMicroseconds(wait);
  }
  else if (dir == 2) {
    // RIGHT
    digitalWrite(dir2, HIGH);
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step2, LOW);
    delayMicroseconds(wait);
  }
  else if (dir == 3) {
    // LEFT
    digitalWrite(dir2, LOW);
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step2, LOW);
    delayMicroseconds(wait);
  }
}


/*
   FUNCTION DESCRIPTION HERE
*/

void kill(void) {
  for (;;);
}

/*
  FUNCTION DESCRIPTION HERE
*/

void readBoard() {
  byte bus;
  for (int j = 0; j < 8; j++){
    digitalWrite(clockPin, HIGH); //First step here will be to pulse the latch/reset pin while sending a falling clock signal.
    digitalWrite(ce, HIGH);
    digitalWrite(latchPin, LOW); //This will lock in the current state of the inputs to be sent to the arduino.
    delayMicroseconds(5);
    digitalWrite(clockPin, LOW);
    digitalWrite(ce, LOW);
    digitalWrite(latchPin, HIGH);
  
    for (int i = 0; i < 8; i++) {
      bitWrite(bus, i, digitalRead(dataPin)); //Grab our byte, select which bit, and write the current input as 1 or 0
  
      digitalWrite(clockPin, LOW);
      delayMicroseconds(5);
      digitalWrite(clockPin, HIGH); //send a clock leading edge so to load the next bit
    } 
    board[j] = bus;
    
  }
  printBoard();
}

/*
  FUNCTION DESCRIPTION HERE
*/

void printBoard() {
  char output[100];

  for (byte i = 0; i < 8; i++) {
    sprintf(output, "%d: %02x\n", i, board[i]);
    Serial.println(output);
  }
  /*
  // New Matrix Stuff
  uint8_t  LEDArray[8];
  myMatrix.clear();
  for (int i = 0; i < 8; i++)
  {
    LEDArray[i] = board[i];
    for (int j = 7; j >= 0; j--)
    {
      if ((LEDArray[i] & 0x01) == 1)
        myMatrix.drawPixel(j, i, 1);
      LEDArray[i] = LEDArray[i] >> 1;
    }
  }
  myMatrix.writeDisplay();
  */
}
