/* Example sketch to control a stepper motor with TB6600 stepper motor driver and Arduino without a library: continuous rotation. More info: https://www.makerguides.com */
/*
 * Step motor must revolve 6.25 times to go 50 mm (1 space)
 * 
 * 
 * 
 */

// Define stepper motor connections:
#define dir1 4
#define step1 5
#define dir2 2
#define step2 3

const int stepsPerSpace = 6.25 * 200; // steps * steps per revolution

char but[] = {9,10,11,12};
int wait = 500;
String serialData;
String prevData;

void setup() {

  Serial.begin(9600);
  // Declare pins as output:
  pinMode(step1, OUTPUT);
  pinMode(dir1, OUTPUT);
  pinMode(step2, OUTPUT);
  pinMode(dir2, OUTPUT);

  pinMode(13, OUTPUT);
  digitalWrite(13, HIGH);

  for(char i = 0; i<4;i++){
    pinMode(but[i], INPUT);
  }
  
}

void loop() {
  
  //if(Serial.available()){
   // wait = Serial.readString().toInt();
  //}

  wait = analogRead(A0);
  //Serial.println(wait);
  int but0 = digitalRead(but[0]);
  int but1 = digitalRead(but[1]);
  int but2 = digitalRead(but[2]);
  int but3 = digitalRead(but[3]);


  if(but0){
    //Serial.print("  Up");
    Move(wait, 0);
  } 
  else if(but1){
    //Serial.print("  Down");
    Move(wait, 1);
  }

  if(but2){
    //Serial.print("  Left");
    for (int i = 0; i < stepsPerSpace; i++)
      Move(wait, 2);
  }
  else if(but3){
    //Serial.print("  Right");
    Move(wait, 3);
  }
  //Serial.println();
  
  
}

void moveSpaces(int spaces1, int spaces2, int d1, int d2) {
  digitalWrite(dir1, d1);
  digitalWrite(dir2, d2);

  while(spaces1 > 0 && spaces2 > 0 { // move in both directions at the same time
    digitalWrite(step1, HIGH);
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    digitalWrite(step2, LOW);
    spaces1--;
    spaces2--;
  }
  while(spaces1 > 0) { // move the rest of the way in direction 1
    digitalWrite(step1, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    spaces1--;
  }
  while(spaces2 > 0) { // move the rest of the way in direction 2
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step2, LOW);
    spaces2--;
  }
}

void Move(int wait, int dir){
  if(wait <=0){
    return;
  }

  if(dir < 0 or dir >= 4){
    return;
  }

  if (dir == 0){
    // UP
    digitalWrite(dir1, HIGH);
    digitalWrite(step1, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    delayMicroseconds(wait);
  }
  else if(dir == 1){
    // DOWN
    digitalWrite(dir1, LOW);
    digitalWrite(step1, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step1, LOW);
    delayMicroseconds(wait);
  }
  else if(dir == 2){
    // RIGHT
    digitalWrite(dir2, HIGH);
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step2, LOW);
    delayMicroseconds(wait);
  }
  else if(dir == 3){
    // LEFT
    digitalWrite(dir2, LOW);
    digitalWrite(step2, HIGH);
    delayMicroseconds(wait);
    digitalWrite(step2, LOW);
    delayMicroseconds(wait);
  }
}
