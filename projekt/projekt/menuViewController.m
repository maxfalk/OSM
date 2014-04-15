//
//  menuViewController.m
//  projekt
//
//  Created by Babak Toghiani-Rizi on 15/04/14.
//  Copyright (c) 2014 OSM-projekt. All rights reserved.
//

#define   DEGREES_TO_RADIANS(degrees)  ((3.14159265359 * (degrees-90))/ 180)
#define OFFSET ((3.14159265359 * (10))/ 180)
#import "menuViewController.h"

@interface menuViewController ()
@property (weak, nonatomic) IBOutlet UILabel *user;

@end

@implementation menuViewController
@synthesize drawpad;


- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    
    //fetch username from server
    _user.text = @"Joel Sandberg";
    

    //fetch win ratio from server
    double winRatio = 0.33;
    
    double winRatioToDegrees = winRatio * 360;
    
    UIBezierPath *path1 = [UIBezierPath bezierPathWithArcCenter:CGPointMake(60, 60)
                                                         radius:50
                                                     startAngle:DEGREES_TO_RADIANS(0)+OFFSET
                                                       endAngle:DEGREES_TO_RADIANS(winRatioToDegrees)-OFFSET
                                                      clockwise:YES];
    
    UIBezierPath *path2 = [UIBezierPath bezierPathWithArcCenter:CGPointMake(60, 60)
                                                         radius:50
                                                     startAngle:DEGREES_TO_RADIANS(winRatioToDegrees)+OFFSET
                                                       endAngle:DEGREES_TO_RADIANS(360)-OFFSET
                                                      clockwise:YES];
    
    UIColor *green = [UIColor colorWithRed:0.41 green:0.72 blue:0.53 alpha:1];
    UIGraphicsBeginImageContext(CGSizeMake(120, 120));
    [[UIColor blackColor] setStroke];
    path1.lineCapStyle = kCGLineCapRound;
    path1.lineWidth = 15.0f;
    [green setStroke];
    [path1 stroke];
    
    UIColor *red = [UIColor colorWithRed:225/255 green:0/255 blue:0/255 alpha:1];
    path2.lineWidth = 15.0f;
    path2.lineCapStyle = kCGLineCapRound;
    [[UIColor redColor] setStroke];
    [path2 stroke];
    self.drawpad.image = UIGraphicsGetImageFromCurrentImageContext();
    
    [self drawRect:CGRectMake(0, 0, 100, 100)];
    
    UIGraphicsEndImageContext();
    
   /* UIBezierPath *path = [[UIBezierPath alloc] init];
    [path moveToPoint:CGPointMake(50.0f, 50.0f)];
    [path addLineToPoint:CGPointMake(270.0f, 50.0f)];
    [path addLineToPoint:CGPointMake(270.0f, 500.0f)];
    [path addLineToPoint:CGPointMake(50.0f, 500.0f)];
    [path closePath];
    UIGraphicsBeginImageContext(self.view.frame.size);
    path.lineCapStyle = kCGLineCapRound;
    path.lineWidth = 1.0f;
    [[UIColor blackColor] setStroke];
    [[UIColor redColor] setFill];
    [path stroke];
    [path fill];
    self.drawpad.image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //[self.view addSubview:self.drawpad];
    */
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (UIBezierPath *)createArcPath
{
    UIBezierPath *aPath = [UIBezierPath bezierPathWithArcCenter:CGPointMake(150, 150)
                                                         radius:75
                                                     startAngle:0
                                                       endAngle:DEGREES_TO_RADIANS(135)
                                                      clockwise:YES];
    
    
    return aPath;

}

- (void)drawRect:(CGRect)rect
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetStrokeColorWithColor(context, [[UIColor blueColor] CGColor]);
    
    UIBezierPath *blueHalf = [UIBezierPath bezierPath];
    [blueHalf addArcWithCenter:CGPointMake(100, 100) radius:90.0 startAngle:-M_PI_2 endAngle:M_PI_2 clockwise:YES];
    [blueHalf setLineWidth:4.0];
    [blueHalf stroke];
    
    CGContextSetStrokeColorWithColor(context, [[UIColor redColor] CGColor]);
    
    UIBezierPath *redHalf = [UIBezierPath bezierPath];
    [redHalf addArcWithCenter:CGPointMake(100.0, 100.0) radius:90.0 startAngle:M_PI_2 endAngle:3.0 * M_PI_2 clockwise:YES];
    [redHalf setLineWidth:4.0];
    [redHalf stroke];
}


- (void)drawCircle
{
    CGPoint center = CGPointMake(100, 100);
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextBeginPath(ctx);
    
    //6 CGContextSetLineWidth(ctx, 5);
    CGContextAddArc(ctx, center.x, center.y, 100.0, 0, 2*M_PI, 0);
    CGContextStrokePath(ctx);
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/
@end
